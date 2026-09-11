// Symbolic Packets represent sets of concrete packets.
// They are represented in a BDD-like structure.
// Unlike traditional BDDs, we do not leave out any levels of the BDD:
// each path down the BDD has precisely the same depth, namely the number of variables, i.e. the packet size in bits.

use rustc_hash::FxHashMap as HashMap;
use std::fmt;


use rand::seq::SliceRandom;

/// A store-local handle with complement polarity in the low bit.
/// SP(0) and SP(1) are the depth-zero constants. Nonterminal handles index
/// `nodes[(handle >> 1) - 1]`; odd handles complement both stored children.
/// Complementation preserves depth, including levels with equal children.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SP(pub u32);

impl SP {
    pub const fn new(value: u32) -> Self {
        SP(value)
    }

    pub fn as_u32(&self) -> u32 {
        self.0
    }

    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for SP {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SP({})", self.0)
    }
}

type Var = u32;

#[inline]
pub(crate) fn pair(a: u32, b: u32) -> u64 { u64::from(a) | (u64::from(b) << 32) }

const EMPTY: u8 = 1;
const UNIVERSAL: u8 = 2;

// Computed results may be evicted; canonical nodes may not. Promote under
// sustained collisions so a fixed cache cannot force unbounded recomputation.
#[derive(Debug, Clone, Default)]
struct UnionCache { entries: Vec<Option<(u64, SP)>>, replacements: usize, overflow: Option<HashMap<u64, SP>> }
impl UnionCache {
    fn slot(key: u64) -> usize {
        use std::hash::Hasher;
        let mut h = rustc_hash::FxHasher::default();
        h.write_u64(key);
        // FxHasher zero-extends a pointer-sized hash on 32-bit targets.
        (h.finish() >> (usize::BITS / 2)) as usize & 1023
    }
    fn get(&self, key: &u64) -> Option<&SP> {
        if let Some(map) = &self.overflow { return map.get(key); }
        let (stored, result) = self.entries.get(Self::slot(*key))?.as_ref()?;
        (*stored == *key).then_some(result)
    }
    fn insert(&mut self, key: u64, result: SP) {
        if let Some(map) = &mut self.overflow { map.insert(key, result); return; }
        if self.entries.is_empty() { self.entries.resize(1024, None); }
        let slot = &mut self.entries[Self::slot(key)];
        if slot.is_some() { self.replacements += 1; }
        *slot = Some((key, result));
        if self.replacements >= 2048 {
            let map = self.entries.iter().flatten().copied().collect();
            self.overflow = Some(map);
            self.entries = Vec::new();
        }
    }
}

/// The store of SPs.
#[derive(Debug, Clone)]
pub struct SPstore {
    num_vars: Var, // Idea: it's ok to pick this larger than you need. Hash consing & memoization will handle it
    // Scalar constants are implicit; every nonterminal level is stored.
    nodes: Vec<SPnode>,
    hc: HashMap<SPnode, SP>,
    pub zero: SP,
    pub one: SP,

    // Memo tables for the operations
    union_memo: UnionCache,
    xor_memo: HashMap<u64, SP>,
    ifelse_memo: HashMap<(Var, SP, SP), SP>,
    properties: Vec<u8>,
    depths: Vec<u32>,
    zeros: Vec<SP>,
    ones: Vec<SP>,
    test_memo: Vec<[Option<SP>; 2]>,
}

/// A node in the SP store. Has two children, one for this variable being 0 and one for it being 1.
/// Stored nodes have an even x0 handle; get() applies the parent polarity.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct SPnode {
    pub x0: SP,
    pub x1: SP,
}

impl std::hash::Hash for SPnode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(pair(self.x0.0, self.x1.0));
    }
}

impl SPstore {
    pub fn new(num_vars: Var) -> Self {
        // All primitive tests need quadratically many full-level prefix nodes.
        let capacity = (num_vars as usize).saturating_mul(num_vars as usize).clamp(16, 4096);
        let mut store = Self {
            num_vars,
            nodes: Vec::with_capacity(capacity),
            hc: HashMap::with_capacity_and_hasher(capacity, Default::default()),
            zero: SP::new(0),
            one: SP::new(0), // Dummy values, will be set later
            union_memo: UnionCache::default(),
            ifelse_memo: HashMap::default(),
            xor_memo: HashMap::default(),
            test_memo: vec![[None; 2]; num_vars as usize],
            properties: { let mut v=Vec::with_capacity(capacity+1);v.push(1);v },
            depths: { let mut v=Vec::with_capacity(capacity+1);v.push(0);v },
            zeros: vec![SP(0)],
            ones: vec![SP(1)],
        };
        store.zero = store.zero();
        store.one = store.one();
        store
    }

    pub fn get(&self, sp: SP) -> SPnode {
        let node = self.nodes[(sp.as_usize() >> 1) - 1];
        let sign = sp.0 & 1;
        SPnode { x0: SP(node.x0.0 ^ sign), x1: SP(node.x1.0 ^ sign) }
    }

    pub fn mk(&mut self, x0: SP, x1: SP) -> SP {
        let sign = x0.0 & 1;
        let node = SPnode { x0: SP(x0.0 ^ sign), x1: SP(x1.0 ^ sign) };
        let base = match self.hc.entry(node) {
            std::collections::hash_map::Entry::Occupied(entry) => *entry.get(),
            std::collections::hash_map::Entry::Vacant(entry) => {
                let id = u32::try_from(self.nodes.len()).expect("node handle overflow")
                    .checked_add(1).and_then(|n| n.checked_mul(2)).expect("node handle overflow");
                let child_depth = self.depths[node.x0.as_usize() >> 1];
                debug_assert_eq!(child_depth, self.depths[node.x1.as_usize() >> 1]);
                self.depths.push(child_depth.checked_add(1).expect("node depth overflow"));
                let p0 = self.properties[node.x0.as_usize() >> 1];
                let p1 = self.properties[node.x1.as_usize() >> 1];
                let p1 = if node.x1.0 & 1 == 0 { p1 } else { ((p1 & EMPTY) << 1) | ((p1 & UNIVERSAL) >> 1) };
                self.properties.push(p0 & p1);
                self.nodes.push(node);
                *entry.insert(SP(id))
            }
        };
        SP(base.0 ^ sign)
    }

    fn properties(&self, value: SP) -> u8 {
        let p = self.properties[value.as_usize() >> 1];
        p << (value.0 & 1)
    }

    pub(crate) fn depth(&self, value: SP) -> u32 { self.depths[value.as_usize() >> 1] }

    pub(crate) fn zero_at_depth(&mut self, depth: u32) -> SP {
        while self.zeros.len() <= depth as usize {
            let z = *self.zeros.last().unwrap();
            let next = self.mk(z, z);
            self.zeros.push(next);
        }
        self.zeros[depth as usize]
    }

    pub(crate) fn one_at_depth(&mut self, depth: u32) -> SP {
        while self.ones.len() <= depth as usize {
            let one = *self.ones.last().unwrap();
            let next = self.mk(one, one);
            self.ones.push(next);
        }
        self.ones[depth as usize]
    }

    fn zero(&mut self) -> SP { self.zero_at_depth(self.num_vars) }
    fn one(&mut self) -> SP { self.one_at_depth(self.num_vars) }

    /// Compile a predicate, returning None for expressions outside the test fragment.
    pub fn compile_predicate(&mut self, expr: &crate::expr::Expr) -> Option<SP> {
        use crate::expr::Expr;
        match expr {
            Expr::Zero => Some(self.zero),
            Expr::One => Some(self.one),
            Expr::Test(field, value) => Some(self.test(*field, *value)),
            Expr::TestNegation(inner) => {
                let p = self.compile_predicate(inner)?;
                Some(self.complement(p))
            }
            Expr::Union(a, b) | Expr::Intersect(a, b) | Expr::Xor(a, b)
            | Expr::Difference(a, b) | Expr::Sequence(a, b) => {
                let a = self.compile_predicate(a)?;
                let b = self.compile_predicate(b)?;
                Some(match expr {
                    Expr::Union(_, _) => self.union(a, b),
                    Expr::Xor(_, _) => self.xor(a, b),
                    Expr::Difference(_, _) => self.difference(a, b),
                    _ => self.intersect(a, b),
                })
            }
            _ => None,
        }
    }

    /// Generates a random SP with `num_vars` variables
    pub fn rand(&mut self) -> SP {
        self.rand_helper(self.num_vars)
    }

    /// Helper function for `rand`: generates a random SP with a certain `depth`
    fn rand_helper(&mut self, depth: Var) -> SP {
        if depth == 0 {
            return if rand::random::<bool>() {
                SP::new(0)
            } else {
                SP::new(1)
            };
        }
        let x0 = self.rand_helper(depth - 1);
        let x1 = self.rand_helper(depth - 1);
        self.mk(x0, x1)
    }

    pub fn union(&mut self, a: SP, b: SP) -> SP {
        debug_assert_eq!(self.depth(a), self.depth(b));
        if a == b { return a; }
        if a.0 == (b.0 ^ 1) { return self.one_at_depth(self.depth(a)); }
        let pa = self.properties(a);
        let pb = self.properties(b);
        if pa & EMPTY != 0 || pb & UNIVERSAL != 0 { return b; }
        if pb & EMPTY != 0 || pa & UNIVERSAL != 0 { return a; }
        let (a, b) = if a.0 > b.0 { (b, a) } else { (a, b) };
        // First, check the memo table
        if let Some(&result) = self.union_memo.get(&pair(a.0, b.0)) {
            return result;
        }
        // We now know that we've got a real node, so we don't need to handle 0 or 1 cases here
        let a_node = self.get(a);
        let b_node = self.get(b);
        let x0 = self.union(a_node.x0, b_node.x0);
        let x1 = self.union(a_node.x1, b_node.x1);
        let res = self.mk(x0, x1);
        self.union_memo.insert(pair(a.0, b.0), res);
        res
    }

    pub fn intersect(&mut self, a: SP, b: SP) -> SP {
        let result = self.union(SP(a.0 ^ 1), SP(b.0 ^ 1)); SP(result.0 ^ 1)
    }

    pub fn complement(&mut self, a: SP) -> SP { SP(a.0 ^ 1) }

    pub fn xor(&mut self, a: SP, b: SP) -> SP {
        debug_assert_eq!(self.depth(a), self.depth(b));
        if a == b { return self.zero_at_depth(self.depth(a)); }
        let pa = self.properties(a);
        let pb = self.properties(b);
        if pa & EMPTY != 0 { return b; }
        if pb & EMPTY != 0 { return a; }
        if pa & UNIVERSAL != 0 { return self.complement(b); }
        if pb & UNIVERSAL != 0 { return self.complement(a); }
        let (a, b) = if a.0 > b.0 { (b, a) } else { (a, b) };
        if let Some(&result) = self.xor_memo.get(&pair(a.0, b.0)) { return result; }
        let x = self.get(a);
        let y = self.get(b);
        let x0 = self.xor(x.x0, y.x0);
        let x1 = self.xor(x.x1, y.x1);
        let result = self.mk(x0, x1);
        self.xor_memo.insert(pair(a.0, b.0), result);
        result
    }

    pub fn difference(&mut self, a: SP, b: SP) -> SP {
        let result = self.union(SP(a.0 ^ 1), b); SP(result.0 ^ 1)
    }

    pub fn is_zero(&mut self, sp: SP) -> bool {
        self.properties(sp) & EMPTY != 0
    }

    pub(crate) fn is_universal(&self, sp: SP) -> bool {
        self.properties(sp) & UNIVERSAL != 0
    }

    pub fn ifelse(&mut self, var: Var, then_branch: SP, else_branch: SP) -> SP {
        assert!(var < self.num_vars);
        self.ifelse_helper(var, then_branch, else_branch)
    }
    fn ifelse_helper(&mut self, var: Var, then_branch: SP, else_branch: SP) -> SP {
        // First, check the memo table
        if let Some(&result) = self.ifelse_memo.get(&(var, then_branch, else_branch)) {
            return result;
        }
        let then_node = self.get(then_branch);
        let else_node = self.get(else_branch);
        let x0;
        let x1;
        if var == 0 {
            x0 = else_node.x0;
            x1 = then_node.x1;
        } else {
            x0 = self.ifelse_helper(var - 1, then_node.x0, else_node.x0);
            x1 = self.ifelse_helper(var - 1, then_node.x1, else_node.x1);
        }
        let res = self.mk(x0, x1);
        self.ifelse_memo
            .insert((var, then_branch, else_branch), res);
        res
    }

    pub fn test(&mut self, var: Var, value: bool) -> SP {
        assert!(var < self.num_vars);
        if let Some(result) = self.test_memo[var as usize][value as usize] {
            return result;
        }
        let depth = self.num_vars - var - 1;
        let zero = self.zero_at_depth(depth);
        let one = SP(zero.0 ^ 1);
        let mut result = self.mk(zero, one);
        for _ in 0..var { result = self.mk(result, result); }
        self.test_memo[var as usize] = [Some(SP(result.0 ^ 1)), Some(result)];
        let result = SP(result.0 ^ u32::from(!value));
        result
    }


    /// Generates a random packet accepted by this SP
    pub fn random_packet(&mut self, sp: SP) -> Option<Vec<bool>> {
        self.random_packet_helper(sp)
    }

    pub fn random_packet_helper(&mut self, sp: SP) -> Option<Vec<bool>> {
        if self.is_zero(sp) {
            return None;
        } else if sp == SP::new(1) {
            return Some(vec![]);
        }
        let node = self.get(sp);
        let mut options = vec![];
        options.push((false, node.x0));
        options.push((true, node.x1));
        // Shuffle the options to randomize
        options.shuffle(&mut rand::rng());
        for (bit_value, child) in options {
            if let Some(mut packet) = self.random_packet_helper(child) {
                packet.insert(0, bit_value);
                return Some(packet);
            }
        }
        None
    }

    /// Enumerates all possible SPs with `num_vars` fields
    pub fn all(&mut self) -> Vec<SP> {
        return self.all_helper(self.num_vars);
    }

    /// Helper function for `all`: enumerates all SPs with `depth` fields
    pub fn all_helper(&mut self, depth: Var) -> Vec<SP> {
        if depth == 0 {
            return vec![SP::new(0), SP::new(1)];
        }
        let all_rec = self.all_helper(depth - 1);
        let mut result = vec![];
        for &x0 in &all_rec {
            for &x1 in &all_rec {
                result.push(self.mk(x0, x1))
            }
        }
        result
    }

    /// Generates a list containing 100 random SPs
    pub fn some(&mut self) -> Vec<SP> {
        let mut result = vec![];
        for _ in 0..100 {
            result.push(self.rand());
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const N: Var = 2;

    #[test]
    fn test_ifelse_selects_branch_by_bit_value() {
        fn contains(s: &SPstore, mut sp: SP, packet: u32) -> bool {
            for var in 0..N {
                let node = s.get(sp);
                sp = if packet & (1 << var) == 0 { node.x0 } else { node.x1 };
            }
            sp == SP::new(1)
        }

        let mut s = SPstore::new(N);
        let all = s.all();
        for var in 0..N {
            for &then_branch in &all {
                for &else_branch in &all {
                    let result = s.ifelse(var, then_branch, else_branch);
                    for packet in 0..(1 << N) {
                        let expected = if packet & (1 << var) == 0 {
                            else_branch
                        } else {
                            then_branch
                        };
                        assert_eq!(contains(&s, result, packet), contains(&s, expected, packet));
                    }
                }
            }
        }
    }

    #[test]
    fn test_matches_packet_program_test() {
        let mut s = crate::spp::SPPstore::new(3);
        for var in 0..3 {
            for value in [false, true] {
                let predicate = s.sp.test(var, value);
                let program = s.test(var, value);
                assert_eq!(predicate, s.fwd(program));
            }
        }
    }

    #[test]
    fn test_laws_0() {
        let mut s = SPstore::new(N);
        assert_eq!(s.complement(s.one), s.zero);
        assert_eq!(s.complement(s.zero), s.one);
    }

    #[test]
    fn test_laws_1() {
        let mut s = SPstore::new(N);
        let all = s.all();
        for sp in all {
            let sp_complement = s.complement(sp);
            let sp2 = s.complement(sp_complement);
            assert_eq!(sp, sp2);

            assert_eq!(s.union(sp, s.zero), sp);
            assert_eq!(s.union(sp, s.one), s.one);
            assert_eq!(s.intersect(sp, s.one), sp);
            assert_eq!(s.intersect(sp, s.zero), s.zero);
        }
    }

    #[test]
    fn test_laws_2() {
        let mut s = SPstore::new(N);
        let all = s.all();
        for &sp1 in &all {
            for &sp2 in &all {
                let sp1_complement = s.complement(sp1);
                let sp2_complement = s.complement(sp2);

                let union = s.union(sp1, sp2);
                let intersect = s.intersect(sp1, sp2);
                let complement_union = s.union(sp1_complement, sp2_complement);
                let complement_intersect = s.intersect(sp1_complement, sp2_complement);

                let union_complement = s.complement(union);
                let intersect_complement = s.complement(intersect);

                assert_eq!(complement_union, intersect_complement);
                assert_eq!(complement_intersect, union_complement);

                let union_rev = s.union(sp2, sp1);
                let intersect_rev = s.intersect(sp2, sp1);
                assert_eq!(union, union_rev);
                assert_eq!(intersect, intersect_rev);

                for i in 0..N {
                    let ifelse = s.ifelse(i, sp1, sp2);
                    let ifelse_complement = s.complement(ifelse);
                    assert_eq!(
                        ifelse_complement,
                        s.ifelse(i, sp1_complement, sp2_complement)
                    );
                }
            }
        }
    }

    #[test]
    fn test_is_zero() {
        let mut s = SPstore::new(N);
        
        // Test base cases
        assert!(s.is_zero(s.zero));
        assert!(!s.is_zero(s.one));
        
        // Test that zero built at any depth is detected as zero
        let mut zero_depth_2 = SP::new(0);
        for _ in 0..2 {
            zero_depth_2 = s.mk(zero_depth_2, zero_depth_2);
        }
        assert!(s.is_zero(zero_depth_2));
        
        // Test a non-zero SP
        let non_zero = s.mk(s.zero, s.one);
        assert!(!s.is_zero(non_zero));
        
        // Test all SPs
        let all = s.all();
        for sp in all {
            let is_zero_result = s.is_zero(sp);
            // An SP is zero if it equals the zero SP
            assert_eq!(is_zero_result, sp == s.zero);
        }
    }
}

#[cfg(test)]
mod complemented_tests {
    use super::*;

    #[test]
    fn complement_shares_nodes_and_preserves_every_level() {
        let mut store = SPstore::new(3);
        for depth in 0..=3 {
            let zero = store.zero_at_depth(depth);
            let one = store.one_at_depth(depth);
            assert_eq!(zero.0 ^ 1, one.0);
            assert_eq!(store.depth(zero), depth);
            assert_eq!(store.depth(one), depth);
        }
        for p in store.all() {
            let count = store.nodes.len();
            let q = store.complement(p);
            assert_eq!(store.nodes.len(), count);
            assert_eq!(store.complement(q), p);
            assert_eq!(store.depth(q), 3);
            let a = store.get(p);
            let b = store.get(q);
            assert_eq!(a.x0.0 ^ 1, b.x0.0);
            assert_eq!(a.x1.0 ^ 1, b.x1.0);
            assert_eq!(store.mk(b.x0, b.x1), q);
            assert_eq!(store.intersect(p, q), store.zero);
            assert_eq!(store.union(p, q), store.one);
            let padded = store.mk(p, p);
            assert_eq!(store.depth(padded), 4);
            assert_eq!(store.get(padded), SPnode { x0: p, x1: p });
        }
        assert!(store.nodes.iter().all(|node| node.x0.0 & 1 == 0));
    }

    #[test]
    fn computed_cache_distributes_keys_across_slots() {
        let slots: std::collections::HashSet<_> = (0..4096)
            .map(|n| UnionCache::slot(pair(2 * n + 2, 2 * n + 4)))
            .collect();
        assert!(slots.len() >= 256, "only {} slots used", slots.len());
    }

    #[test]
    fn computed_cache_collisions_never_return_another_key() {
        let mut cache = UnionCache::default();
        for key in 0..10000 {
            cache.insert(key, SP(key as u32));
            assert_eq!(cache.get(&key), Some(&SP(key as u32)));
            if let Some(value) = cache.get(&(key / 2)) { assert_eq!(*value, SP((key / 2) as u32)); }
        }
        assert!(cache.overflow.is_some());
    }
}
