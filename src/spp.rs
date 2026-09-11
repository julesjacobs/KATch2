// Symbolic Packet Programs represent relations of concrete packets.
// They are represented in a BDD-like structure.
// Unlike traditional BDDs, we do not leave out any levels of the BDD:
// each path down the BDD has precisely the same depth, namely the number of variables, i.e. the packet size in bits.

use rand::seq::SliceRandom;

use crate::sp::{pair, SPnode, SPstore, SP};
#[allow(non_snake_case)]
use rustc_hash::{FxBuildHasher, FxHashMap as HashMap};
use std::hash::BuildHasher;

/// We use indices into the SPP store to represent SPPs.
/// The zero SPP is represented by SPP(0) and the one SPP is represented by SPP(1).
/// Indices into the store are the u32 value - 2.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SPP(pub u32);

impl SPP {
    pub const fn new(value: u32) -> Self {
        SPP(value)
    }

    pub fn as_u32(&self) -> u32 {
        self.0
    }

    pub fn as_usize(&self) -> usize {
        self.0 as usize
    }
}

impl std::fmt::Display for SPP {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "SPP({})", self.0)
    }
}

pub type Var = u32;

const EMPTY: u8 = 1;
const IDENTITY: u8 = 2;
const UNIVERSAL: u8 = 4;

/// The store of SPPs. (store = arena + memo tables)
#[derive(Debug)]
pub struct SPPstore {
    num_vars: Var, // Idea: it's ok to pick this larger than you need. Hash consing & memoization will handle it
    nodes: Vec<SPPnode>, // the arena
    hc: hashbrown::HashTable<SPP>,
    pub zero: SPP,
    pub one: SPP,
    pub top: SPP,

    // Memo tables for the operations
    union_memo: HashMap<u64, SPP>,
    intersect_memo: HashMap<u64, SPP>,
    xor_memo: HashMap<u64, SPP>,
    difference_memo: HashMap<u64, SPP>,
    sequence_memo: HashMap<u64, SPP>,
    star_memo: HashMap<SPP, SPP>,
    complement_memo: HashMap<SPP, SPP>,
    // branch_memo: HashMap<(Var, SPP, SPP, SPP, SPP), SPP>,
    test_memo: Vec<[Option<SPP>; 2]>,
    assign_memo: Vec<[Option<SPP>; 2]>,
    flip_memo: HashMap<SPP, SPP>,
    properties: Vec<u8>,
    depths: Vec<u32>,
    zeros: Vec<SPP>,
    ones: Vec<SPP>,
    tops: Vec<SPP>,

    pub sp: SPstore,
    fwd_memo: HashMap<SPP, SP>,
    ifwd_memo: HashMap<SP, SPP>,
    bwd_memo: HashMap<SPP, SP>,
    push_memo: HashMap<u64, SP>,
    pull_memo: HashMap<u64, SP>,
    has_image_memo: HashMap<u64, bool>,
    diagonal_memo: HashMap<SP, SPP>,
}

/// A node in the SPP store. Has four children, one for each combination of the two variables.
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct SPPnode {
    pub x00: SPP,
    pub x01: SPP,
    pub x10: SPP,
    pub x11: SPP,
}

impl std::hash::Hash for SPPnode {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(pair(self.x00.0, self.x01.0));
        state.write_u64(pair(self.x10.0, self.x11.0));
    }
}

impl SPPstore {
    pub fn new(num_vars: Var) -> Self {
        let mut store = Self {
            num_vars,
            nodes: vec![],
            hc: hashbrown::HashTable::new(),
            zero: SPP::new(0),
            one: SPP::new(0),
            top: SPP::new(0), // Dummy values, will be set later
            union_memo: HashMap::default(),
            intersect_memo: HashMap::default(),
            xor_memo: HashMap::default(),
            difference_memo: HashMap::default(),
            sequence_memo: HashMap::default(),
            star_memo: HashMap::default(),
            complement_memo: HashMap::default(),
            // branch_memo: HashMap::default(),
            test_memo: vec![[None; 2]; num_vars as usize],
            assign_memo: vec![[None; 2]; num_vars as usize],
            flip_memo: HashMap::default(),
            properties: vec![1, 6],
            depths: vec![0, 0],
            zeros: vec![SPP(0)],
            ones: vec![SPP(1)],
            tops: vec![SPP(1)],
            sp: SPstore::new(num_vars),

            fwd_memo: HashMap::default(),
            ifwd_memo: HashMap::default(),
            has_image_memo: HashMap::default(),
            diagonal_memo: HashMap::default(),
            bwd_memo: HashMap::default(),
            push_memo: HashMap::default(),
            pull_memo: HashMap::default(),
        };
        store.zero = store.zero();
        store.one = store.one();
        store.top = store.top();

        store
    }

    /// Retrieves the SPPnode corresponding to a given SPP index.
    /// Panics if the index is 0, 1, or out of bounds.
    /// Assumes the caller ensures the index represents an internal node.
    pub fn get(&self, spp: SPP) -> SPPnode {
        assert!(spp.as_u32() >= 2, "Cannot call get on SPP 0 or 1");
        let node_index = (spp.as_u32() - 2) as usize;
        // Use the variable to make the assertion clearer
        assert!(
            node_index < self.nodes.len(),
            "SPP index out of bounds: index {}, len {}",
            node_index,
            self.nodes.len()
        );
        self.nodes[node_index]
    }

    /// Computes the possible output packet set from applying the `SPP`
    pub fn fwd(&mut self, spp: SPP) -> SP {
        if spp.0 < 2 { return SP(spp.0); }
        // Check the memo table to see if fwd(spp) already exists
        if let Some(&result) = self.fwd_memo.get(&spp) {
            return result;
        }

        // We now know that we've got a non-trivial SPPNode,
        // so we don't need to handle 0 or 1 cases here

        let SPPnode { x00, x01, x10, x11 } = self.get(spp);

        let f00 = self.fwd(x00);
        let f10 = self.fwd(x10);
        let f01 = self.fwd(x01);
        let f11 = self.fwd(x11);
        let x0 = self.sp.union(f00, f10);
        let x1 = self.sp.union(f01, f11);
        let result = self.sp.mk(x0, x1);
        self.fwd_memo.insert(spp, result);
        result
    }

    /// Computes the set of packets, which when input to the `spp`,
    /// produce some non-empty set of packets as output
    pub fn bwd(&mut self, spp: SPP) -> SP {
        if spp.0 < 2 { return SP(spp.0); }
        if let Some(&result) = self.bwd_memo.get(&spp) {
            return result;
        }
        let SPPnode { x00, x01, x10, x11 } = self.get(spp);
        let b00 = self.bwd(x00);
        let b01 = self.bwd(x01);
        let b10 = self.bwd(x10);
        let b11 = self.bwd(x11);
        let x0 = self.sp.union(b00, b01);
        let x1 = self.sp.union(b10, b11);
        let result = self.sp.mk(x0, x1);
        self.bwd_memo.insert(spp, result);
        result
    }

    /// Computes the SPP corresponding to the `sp` returned by `fwd`.     
    /// - `ifwd` is the right inverse of `fwd`, i.e. `fwd ∘ ifwd = id_SP`
    pub fn ifwd(&mut self, sp: SP) -> SPP {
        if sp.0 < 2 { return SPP(sp.0); }
        // Check the memo table to see if ifwd(spp) already exists
        if let Some(&result) = self.ifwd_memo.get(&sp) {
            return result;
        }

        let SPnode { x0, x1 } = self.sp.get(sp);
        let x00 = self.ifwd(x0);
        let x01 = self.ifwd(x1);
        let x10 = x00;
        let x11 = x01;
        let result = self.mk(x00, x01, x10, x11);
        self.ifwd_memo.insert(sp, result);
        result
    }

    /// Computes the SPP corresponding to the `sp` returned by `bwd`.            
    /// - `ibwd` is the left inverse of `bwd`, i.e. `ibwd ∘ bwd = id`
    pub fn ibwd(&mut self, sp: SP) -> SPP {
        let spp = self.ifwd(sp);
        self.flip(spp)
    }

    pub fn mk(&mut self, x00: SPP, x01: SPP, x10: SPP, x11: SPP) -> SPP {
        let node = SPPnode { x00, x01, x10, x11 };

        *self.hc.entry(
            FxBuildHasher.hash_one(node),
            |id| self.nodes[id.as_usize() - 2] == node,
            |id| FxBuildHasher.hash_one(self.nodes[id.as_usize() - 2]),
        ).or_insert_with(|| {
            let id = u32::try_from(self.nodes.len()).expect("node handle overflow").checked_add(2).expect("node handle overflow");
            let result = SPP::new(id);
            let child_depth = self.depths[x00.as_usize()];
            debug_assert_eq!(child_depth, self.depths[x01.as_usize()]);
            debug_assert_eq!(child_depth, self.depths[x10.as_usize()]);
            debug_assert_eq!(child_depth, self.depths[x11.as_usize()]);
            self.depths.push(child_depth.checked_add(1).expect("node depth overflow"));
            let p00 = self.properties[x00.as_usize()];
            let p01 = self.properties[x01.as_usize()];
            let p10 = self.properties[x10.as_usize()];
            let p11 = self.properties[x11.as_usize()];
            let mut properties = (p00 & p01 & p10 & p11) & (EMPTY | UNIVERSAL);
            if p00 & p11 & IDENTITY != 0 && p01 & p10 & EMPTY != 0 {
                properties |= IDENTITY;
            }
            self.properties.push(properties);
            self.nodes.push(node);
            result
        }).get()
    }

    pub(crate) fn depth(&self, value: SPP) -> u32 { self.depths[value.as_usize()] }

    pub(crate) fn zero_at_depth(&mut self, depth: u32) -> SPP {
        while self.zeros.len() <= depth as usize {
            let z = *self.zeros.last().unwrap();
            let next = self.mk(z, z, z, z);
            self.zeros.push(next);
        }
        self.zeros[depth as usize]
    }

    pub(crate) fn one_at_depth(&mut self, depth: u32) -> SPP {
        while self.ones.len() <= depth as usize {
            let one = *self.ones.last().unwrap();
            let zero = self.zero_at_depth(self.ones.len() as u32 - 1);
            let next = self.mk(one, zero, zero, one);
            self.ones.push(next);
        }
        self.ones[depth as usize]
    }

    fn zero(&mut self) -> SPP { self.zero_at_depth(self.num_vars) }
    fn one(&mut self) -> SPP { self.one_at_depth(self.num_vars) }

    fn top_at_depth(&mut self, depth: u32) -> SPP {
        while self.tops.len() <= depth as usize {
            let top = *self.tops.last().unwrap();
            let next = self.mk(top, top, top, top);
            self.tops.push(next);
        }
        self.tops[depth as usize]
    }

    fn top(&mut self) -> SPP { self.top_at_depth(self.num_vars) }

    pub fn union(&mut self, a: SPP, b: SPP) -> SPP {
        debug_assert_eq!(self.depth(a), self.depth(b));
        if a == b { return a; }
        let pa = self.properties[a.as_usize()];
        let pb = self.properties[b.as_usize()];
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
        let x00 = self.union(a_node.x00, b_node.x00);
        let x01 = self.union(a_node.x01, b_node.x01);
        let x10 = self.union(a_node.x10, b_node.x10);
        let x11 = self.union(a_node.x11, b_node.x11);
        let res = self.mk(x00, x01, x10, x11);
        self.union_memo.insert(pair(a.0, b.0), res);
        res
    }

    pub fn intersect(&mut self, a: SPP, b: SPP) -> SPP {
        debug_assert_eq!(self.depth(a), self.depth(b));
        if a == b { return a; }
        let pa = self.properties[a.as_usize()];
        let pb = self.properties[b.as_usize()];
        if pa & EMPTY != 0 || pb & UNIVERSAL != 0 { return a; }
        if pb & EMPTY != 0 || pa & UNIVERSAL != 0 { return b; }
        let (a, b) = if a.0 > b.0 { (b, a) } else { (a, b) };
        // First, check the memo table
        if let Some(&result) = self.intersect_memo.get(&pair(a.0, b.0)) {
            return result;
        }
        let a_node = self.get(a);
        let b_node = self.get(b);
        let x00 = self.intersect(a_node.x00, b_node.x00);
        let x01 = self.intersect(a_node.x01, b_node.x01);
        let x10 = self.intersect(a_node.x10, b_node.x10);
        let x11 = self.intersect(a_node.x11, b_node.x11);
        let res = self.mk(x00, x01, x10, x11);
        self.intersect_memo.insert(pair(a.0, b.0), res);
        res
    }

    pub fn xor(&mut self, a: SPP, b: SPP) -> SPP {
        if a == b { return self.zero_at_depth(self.depth(a)); }
        debug_assert_eq!(self.depth(a), self.depth(b));
        let pa = self.properties[a.as_usize()];
        let pb = self.properties[b.as_usize()];
        if pa & EMPTY != 0 { return b; }
        if pb & EMPTY != 0 { return a; }
        if pa & UNIVERSAL != 0 { return self.complement(b); }
        if pb & UNIVERSAL != 0 { return self.complement(a); }
        let (a, b) = if a.0 > b.0 { (b, a) } else { (a, b) };
        // First, check the memo table
        if let Some(&result) = self.xor_memo.get(&pair(a.0, b.0)) {
            return result;
        }
        let a_node = self.get(a);
        let b_node = self.get(b);
        let x00 = self.xor(a_node.x00, b_node.x00);
        let x01 = self.xor(a_node.x01, b_node.x01);
        let x10 = self.xor(a_node.x10, b_node.x10);
        let x11 = self.xor(a_node.x11, b_node.x11);
        let res = self.mk(x00, x01, x10, x11);
        self.xor_memo.insert(pair(a.0, b.0), res);
        res
    }

    pub fn difference(&mut self, a: SPP, b: SPP) -> SPP {
        if a == b { return self.zero_at_depth(self.depth(a)); }
        debug_assert_eq!(self.depth(a), self.depth(b));
        let pa = self.properties[a.as_usize()];
        let pb = self.properties[b.as_usize()];
        if pa & EMPTY != 0 || pb & EMPTY != 0 { return a; }
        if pa & UNIVERSAL != 0 { return self.complement(b); }
        // First, check the memo table
        if let Some(&result) = self.difference_memo.get(&pair(a.0, b.0)) {
            return result;
        }
        // Difference a - b is defined as a & !b.
        // We could implement it that way, but recursive definition is simpler here.
        let a_node = self.get(a);
        let b_node = self.get(b);
        let x00 = self.difference(a_node.x00, b_node.x00);
        let x01 = self.difference(a_node.x01, b_node.x01);
        let x10 = self.difference(a_node.x10, b_node.x10);
        let x11 = self.difference(a_node.x11, b_node.x11);
        let res = self.mk(x00, x01, x10, x11);
        self.difference_memo.insert(pair(a.0, b.0), res); // Insert result into memo table
        res
    }

    pub fn complement(&mut self, a: SPP) -> SPP {
        if a.0 < 2 { return SPP(1 - a.0); }
        // First, check the memo table
        if let Some(&result) = self.complement_memo.get(&a) {
            return result;
        }
        let node = self.get(a);
        let x00 = self.complement(node.x00);
        let x01 = self.complement(node.x01);
        let x10 = self.complement(node.x10);
        let x11 = self.complement(node.x11);
        let res = self.mk(x00, x01, x10, x11);
        self.complement_memo.insert(a, res);
        res
    }

    /// Checks if an SPP is zero (represents the empty relation)
    pub fn is_zero(&mut self, spp: SPP) -> bool {
        self.properties[spp.as_usize()] & EMPTY != 0
    }

    pub fn sequence(&mut self, a: SPP, b: SPP) -> SPP {
        debug_assert_eq!(self.depth(a), self.depth(b));
        let pa = self.properties[a.as_usize()];
        let pb = self.properties[b.as_usize()];
        if pa & EMPTY != 0 || pb & IDENTITY != 0 { return a; }
        if pb & EMPTY != 0 || pa & IDENTITY != 0 { return b; }
        // First, check the memo table
        if let Some(&result) = self.sequence_memo.get(&pair(a.0, b.0)) {
            return result;
        }
        // We now know that we've got a real node, so we don't need to handle 0 or 1 cases here
        let a_node = self.get(a);
        let b_node = self.get(b);
        // This is like matrix multiplication
        // (a00, a01; a10, a11) * (b00, b01; b10, b11) = (a00*b00 + a01*b10, a00*b01 + a01*b11; a10*b00 + a11*b10, a10*b01 + a11*b11)
        // Pictorially:
        //                      b00 b01
        //                      b10 b11
        //
        // a00 a01      a00b00 + a01b10  a00b01 + a01b11
        // a10 a11      a10b00 + a11b10  a10b01 + a11b11
        let a00b00 = self.sequence(a_node.x00, b_node.x00);
        let a01b10 = self.sequence(a_node.x01, b_node.x10);
        let a00b01 = self.sequence(a_node.x00, b_node.x01);
        let a01b11 = self.sequence(a_node.x01, b_node.x11);
        let a10b00 = self.sequence(a_node.x10, b_node.x00);
        let a11b10 = self.sequence(a_node.x11, b_node.x10);
        let a10b01 = self.sequence(a_node.x10, b_node.x01);
        let a11b11 = self.sequence(a_node.x11, b_node.x11);
        let x00 = self.union(a00b00, a01b10);
        let x01 = self.union(a00b01, a01b11);
        let x10 = self.union(a10b00, a11b10);
        let x11 = self.union(a10b01, a11b11);
        let res = self.mk(x00, x01, x10, x11);
        self.sequence_memo.insert(pair(a.0, b.0), res);
        res
    }

    /// `push` computes the effect of an SPP on an SP, returning the new SP.    
    /// The new SP contains all packets that are produced when the `spp`
    /// is applied on the `sp`.
    pub fn push(&mut self, sp: SP, spp: SPP) -> SP {
        if sp.0 < 2 && spp.0 < 2 { return SP(sp.0 & spp.0); }
        if self.sp.is_zero(sp) || self.properties[spp.as_usize()] & IDENTITY != 0 {
            return sp;
        }
        if let Some(&result) = self.push_memo.get(&pair(sp.0, spp.0)) {
            return result;
        }
        let result = if self.sp.is_universal(sp) {
            self.fwd(spp)
        } else {
            let p = self.sp.get(sp);
            let r = self.get(spp);
            let a = self.push(p.x0, r.x00);
            let b = self.push(p.x1, r.x10);
            let c = self.push(p.x0, r.x01);
            let d = self.push(p.x1, r.x11);
            let x0 = self.sp.union(a, b);
            let x1 = self.sp.union(c, d);
            self.sp.mk(x0, x1)
        };
        self.push_memo.insert(pair(sp.0, spp.0), result);
        result
    }

    /// A concrete packet `α ∈ pull(spp, sp)` iff running `spp` on `α`
    /// produces an output packet in the `sp`.   
    /// In other words, `pull` simulates the backward transition of an SP
    /// over the SP (i.e. `pull` simulates the effect of an SPP in reverse).
    pub fn pull(&mut self, spp: SPP, sp: SP) -> SP {
        if sp.0 < 2 && spp.0 < 2 { return SP(sp.0 & spp.0); }
        if self.sp.is_zero(sp) || self.properties[spp.as_usize()] & IDENTITY != 0 {
            return sp;
        }
        if let Some(&result) = self.pull_memo.get(&pair(spp.0, sp.0)) {
            return result;
        }
        let result = if self.sp.is_universal(sp) {
            self.bwd(spp)
        } else {
            let p = self.sp.get(sp);
            let r = self.get(spp);
            let a = self.pull(r.x00, p.x0);
            let b = self.pull(r.x10, p.x0);
            let c = self.pull(r.x01, p.x1);
            let d = self.pull(r.x11, p.x1);
            let x0 = self.sp.union(a, c);
            let x1 = self.sp.union(b, d);
            self.sp.mk(x0, x1)
        };
        self.pull_memo.insert(pair(spp.0, sp.0), result);
        result
    }

    /// The filter relation {(packet, packet) | packet is in predicate}.
    pub fn diagonal(&mut self, predicate: SP) -> SPP {
        if predicate.0 < 2 { return SPP(predicate.0); }
        if let Some(&result) = self.diagonal_memo.get(&predicate) { return result; }
        let p = self.sp.get(predicate);
        let x0 = self.diagonal(p.x0);
        let x1 = self.diagonal(p.x1);
        let zero = self.zero_at_depth(self.sp.depth(predicate) - 1);
        let result = self.mk(x0, zero, zero, x1);
        self.diagonal_memo.insert(predicate, result);
        result
    }

    pub fn has_image(&mut self, input: SP, relation: SPP) -> bool {
        if self.sp.is_zero(input) || self.is_zero(relation) { return false; }
        if self.sp.is_universal(input) || self.properties[relation.as_usize()] & (IDENTITY | UNIVERSAL) != 0 {
            return true;
        }
        if let Some(&result) = self.has_image_memo.get(&pair(input.0, relation.0)) { return result; }
        let p = self.sp.get(input);
        let r = self.get(relation);
        let result = self.has_image(p.x0, r.x00) || self.has_image(p.x0, r.x01)
            || self.has_image(p.x1, r.x10) || self.has_image(p.x1, r.x11);
        self.has_image_memo.insert(pair(input.0, relation.0), result);
        result
    }

    pub fn star(&mut self, x: SPP) -> SPP {
        let properties = self.properties[x.as_usize()];
        if properties & EMPTY != 0 { return self.one_at_depth(self.depth(x)); }
        if properties & (IDENTITY | UNIVERSAL) != 0 { return x; }
        // First, check the memo table
        if let Some(&result) = self.star_memo.get(&x) {
            return result;
        }
        let x_node = self.get(x);
        // Compute the Kleene star of a as seen as a matrix (a00, a01; a10, a11)
        let a = x_node.x00;
        let b = x_node.x01;
        let c = x_node.x10;
        let d = x_node.x11;
        let d_star = self.star(d);
        let bd_star = self.sequence(b, d_star);
        let bd_star_c = self.sequence(bd_star, c);
        let a_plus_bdc = self.union(a, bd_star_c);
        let res_a = self.star(a_plus_bdc);
        let res_a_bd_star = self.sequence(res_a, bd_star);
        let res_b = res_a_bd_star;
        let c_res_a = self.sequence(c, res_a);
        let d_star_c_res_a = self.sequence(d_star, c_res_a);
        let res_c = d_star_c_res_a;
        let res_c_bd_star = self.sequence(res_c, bd_star);
        let res_d = self.union(d_star, res_c_bd_star);
        let res = self.mk(res_a, res_b, res_c, res_d);
        self.star_memo.insert(x, res);
        res
    }

    pub fn test(&mut self, var: Var, value: bool) -> SPP {
        assert!(var < self.num_vars);
        if let Some(result) = self.test_memo[var as usize][value as usize] {
            return result;
        }
        let mut res = SPP::new(1);
        let mut zero = SPP::new(0);
        for i in (0..self.num_vars).rev() {
            if i == var {
                if value {
                    res = self.mk(zero, zero, zero, res);
                } else {
                    res = self.mk(res, zero, zero, zero);
                }
            } else {
                res = self.mk(res, zero, zero, res);
            }
            zero = self.mk(zero, zero, zero, zero);
        }
        self.test_memo[var as usize][value as usize] = Some(res);
        res
    }

    pub fn assign(&mut self, var: Var, value: bool) -> SPP {
        assert!(var < self.num_vars);
        if let Some(result) = self.assign_memo[var as usize][value as usize] {
            return result;
        }
        let mut res = SPP::new(1);
        let mut zero = SPP::new(0);
        for i in (0..self.num_vars).rev() {
            if i == var {
                if value {
                    res = self.mk(zero, res, zero, res);
                } else {
                    res = self.mk(res, zero, res, zero);
                }
            } else {
                res = self.mk(res, zero, zero, res);
            }
            zero = self.mk(zero, zero, zero, zero);
        }
        self.assign_memo[var as usize][value as usize] = Some(res);
        res
    }

    /// Computes all packets that can be produced from this SPP.
    /// We give the answer as an SPP instead of an SP for convenience.
    /// **Note**: this method has been deprecated in favor of `fwd`
    pub fn naive_forward(&mut self, spp: SPP) -> SPP {
        let top = self.top_at_depth(self.depth(spp));
        self.sequence(top, spp)
    }

    /// Computes all packets that can be input to this SPP.
    pub fn backward(&mut self, spp: SPP) -> SPP {
        let top = self.top_at_depth(self.depth(spp));
        self.sequence(spp, top)
    }

    /// Flips the relation represented by this SPP.
    pub fn flip(&mut self, spp: SPP) -> SPP {
        if spp.0 < 2 { return SPP(spp.0); }
        if let Some(&result) = self.flip_memo.get(&spp) {
            return result;
        }
        let spp_node = self.get(spp);

        // Recursively flip each of the children of `spp_node`
        let f00 = self.flip(spp_node.x00);
        let f01 = self.flip(spp_node.x01);
        let f10 = self.flip(spp_node.x10);
        let f11 = self.flip(spp_node.x11);

        let res = self.mk(f00, f10, f01, f11);
        self.flip_memo.insert(spp, res);
        res
    }

    pub fn random_packet_pair(&mut self, spp: SPP) -> Option<(Vec<bool>, Vec<bool>)> {
        return self.random_packet_pair_helper(spp);
    }

    fn random_packet_pair_helper(&mut self, spp: SPP) -> Option<(Vec<bool>, Vec<bool>)> {
        if self.is_zero(spp) {
            return None;
        } else if spp == SPP::new(1) {
            return Some((vec![], vec![]));
        }
        let spp_node = self.get(spp);
        let mut options = vec![];
        options.push((false, false, spp_node.x00));
        options.push((false, true, spp_node.x01));
        options.push((true, false, spp_node.x10));
        options.push((true, true, spp_node.x11));
        // Shuffle the options
        options.shuffle(&mut rand::rng());
        for (b1, b2, child) in options {
            let random_packet_pair = self.random_packet_pair_helper(child);
            if let Some((v1, v2)) = random_packet_pair {
                let mut w1 = v1.clone();
                w1.insert(0, b1);
                let mut w2 = v2.clone();
                w2.insert(0, b2);
                return Some((w1, w2));
            }
        }
        None
    }

    pub fn random_input_packet(&mut self, spp: SPP) -> Option<Vec<bool>> {
        // get a random packet pair, then return first element
        let random_packet_pair = self.random_packet_pair(spp);
        if let Some((v1, _)) = random_packet_pair {
            return Some(v1);
        }
        None
    }

    pub fn random_output_packet_from_input(&mut self, spp: SPP, input: Vec<bool>) -> Option<Vec<bool>> {
        return self.random_output_packet_from_input_helper(spp, input);
    }
    
    fn random_output_packet_from_input_helper(&mut self, spp: SPP, input: Vec<bool>) -> Option<Vec<bool>> {
        if self.is_zero(spp) {
            return None;
        } else if spp == SPP::new(1) {
            assert!(input.len() == 0);
            return Some(vec![]);
        }
        let spp_node = self.get(spp);
        let mut options = vec![];
        if !input[0] {
            options.push((false, false, spp_node.x00));
            options.push((false, true, spp_node.x01));
        } else {
            options.push((true, false, spp_node.x10));
            options.push((true, true, spp_node.x11));
        }
        // Remove first element from input
        let mut rest = input.clone();
        rest.remove(0);
        // Shuffle the options
        options.shuffle(&mut rand::rng());
        for (_b1, b2, child) in options {
            let random_packet = self.random_output_packet_from_input_helper(child, rest.clone());
            if let Some(v2) = random_packet {
                let mut w2 = v2.clone();
                w2.insert(0, b2);
                return Some(w2);
            }
        }
        None
    }


    /// Enumerates all possible SPPs with `num_vars` fields
    #[cfg(test)]
    pub fn all(&mut self) -> Vec<SPP> {
        return self.all_helper(self.num_vars);
    }

    /// Helper function for `all`: enumerates all SPPs with `depth` fields
    #[cfg(test)]
    fn all_helper(&mut self, depth: Var) -> Vec<SPP> {
        if depth == 0 {
            return vec![SPP::new(0), SPP::new(1)];
        }
        let all_rec = self.all_helper(depth - 1);
        let mut result = vec![];
        for &x00 in &all_rec {
            for &x01 in &all_rec {
                for &x10 in &all_rec {
                    for &x11 in &all_rec {
                        result.push(self.mk(x00, x01, x10, x11))
                    }
                }
            }
        }
        result
    }

    /// Generates a random SPP with `num_vars` variables
    #[cfg(test)]
    pub fn rand(&mut self) -> SPP {
        self.rand_helper(self.num_vars)
    }

    /// Helper function for `rand`: generates a random SPP with a certain `depth`
    #[cfg(test)]
    fn rand_helper(&mut self, depth: Var) -> SPP {
        if depth == 0 {
            return if rand::random::<f64>() < 0.75 {
                SPP::new(0)
            } else {
                SPP::new(1)
            };
        }
        let x00 = self.rand_helper(depth - 1);
        let x01 = self.rand_helper(depth - 1);
        let x10 = self.rand_helper(depth - 1);
        let x11 = self.rand_helper(depth - 1);
        self.mk(x00, x01, x10, x11)
    }

    /// Generates a list containing 100 random SPPs
    #[cfg(test)]
    pub fn some(&mut self) -> Vec<SPP> {
        let mut result = vec![];
        for _ in 0..100 {
            result.push(self.rand());
        }
        result
    }
    
    /// Returns the number of nodes in the SPP store
    pub fn num_nodes(&self) -> usize {
        self.nodes.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const N: Var = 2;

    /// Test that `naive_forward` and `fwd` behave the same
    #[test]
    fn test_naive_forward_fwd_agree() {
        let mut s = SPPstore::new(N);
        // iterate over all possible SPPs over `N` variables
        let all = s.all();
        for spp in all {
            let naive_spp: SPP = s.naive_forward(spp);
            let sp: SP = s.fwd(spp);
            let candidate_spp: SPP = s.ifwd(sp);
            assert_eq!(naive_spp, candidate_spp);
        }
    }

    /// Test that `fwd ∘ ifwd = id_SP`
    #[test]
    fn test_fwd_ifwd_is_identity() {
        let mut s = SPPstore::new(N);
        for sp in s.sp.all() {
            let ifwd_sp: SPP = s.ifwd(sp);
            let result: SP = s.fwd(ifwd_sp);
            assert_eq!(sp, result, "{:?} and {:?} are different SPs", sp, result);
        }
    }

    /// Tests that for random SP, SPPs, `push(sp, spp) == pull(spp.flip(), sp)`
    #[test]
    fn test_push_pull() {
        let mut s = SPPstore::new(N);
        for spp in s.all() {
            let sp: SP = s.sp.rand();
            let push_result: SP = s.push(sp, spp);
            let flipped_spp: SPP = s.flip(spp);
            let pull_result: SP = s.pull(flipped_spp, sp);
            assert_eq!(
                push_result, pull_result,
                "{:?} and {:?} are different SPs",
                push_result, pull_result
            );
        }
    }

    /// Test that `(ifwd ∘ fwd)(Top; SPP) = Top; SPP`
    #[test]
    fn test_ifwd_fwd_is_identity() {
        let mut s = SPPstore::new(N);
        for spp in s.all() {
            // Sequence on the left with top to clear fields from the SPP
            let seq_spp: SPP = s.sequence(s.top, spp);
            let sp: SP = s.fwd(seq_spp);
            let result: SPP = s.ifwd(sp);
            assert_eq!(
                seq_spp, result,
                "{} and {} are different SPPs",
                seq_spp, result
            );
        }
    }

    #[test]
    fn test_laws_0() {
        let mut s = SPPstore::new(N);
        assert_eq!(s.complement(s.top), s.zero);
        assert_eq!(s.complement(s.zero), s.top);
    }

    #[test]
    fn test_laws_1() {
        let mut s = SPPstore::new(N);
        let all = s.all();
        for spp in all {
            let spp_complement = s.complement(spp);
            let spp2 = s.complement(spp_complement);
            assert_eq!(spp, spp2);

            assert_eq!(s.union(spp, s.zero), spp);
            assert_eq!(s.union(spp, s.top), s.top);
            assert_eq!(s.intersect(spp, s.top), spp);
            assert_eq!(s.intersect(spp, s.zero), s.zero);

            let spp_seq_one = s.sequence(spp, s.one);
            let spp_seq_zero = s.sequence(spp, s.zero);
            let one_seq_spp = s.sequence(s.one, spp);
            let zero_seq_spp = s.sequence(s.zero, spp);
            assert_eq!(spp_seq_one, spp);
            assert_eq!(spp_seq_zero, s.zero);
            assert_eq!(one_seq_spp, spp);
            assert_eq!(zero_seq_spp, s.zero);

            // Test Kleene algebra laws for star
            let spp_star = s.star(spp);
            let spp_star_star = s.star(spp_star);
            assert_eq!(spp_star_star, spp_star);

            let spp_union_star = s.union(s.one, spp_star);
            assert_eq!(spp_star, spp_union_star);

            // Test that star(star(x)) = star(x)
            let spp_star_star = s.star(spp_star);
            assert_eq!(spp_star, spp_star_star);

            // Test that star(0) = 1
            assert_eq!(s.star(s.zero), s.one);

            // Test that star(1) = 1
            assert_eq!(s.star(s.one), s.one);

            // Test that x* = 1 + x·x*
            let spp_seq_star = s.sequence(spp, spp_star);
            let one_union_seq_star = s.union(s.one, spp_seq_star);
            assert_eq!(spp_star, one_union_seq_star);

            // Test that x* = 1 + x*·x
            let spp_seq_star_seq_star = s.sequence(spp_star, spp);
            let one_union_seq_star_seq_star = s.union(s.one, spp_seq_star_seq_star);
            assert_eq!(spp_star, one_union_seq_star_seq_star);
        }
    }

    #[test]
    fn test_laws_2() {
        let mut s = SPPstore::new(N);
        for &spp1 in &s.some() {
            for &spp2 in &s.some() {
                let spp1_complement = s.complement(spp1);
                let spp2_complement = s.complement(spp2);

                let union = s.union(spp1, spp2);
                let intersect = s.intersect(spp1, spp2);
                let complement_union = s.union(spp1_complement, spp2_complement);
                let complement_intersect = s.intersect(spp1_complement, spp2_complement);

                let union_complement = s.complement(union);
                let intersect_complement = s.complement(intersect);

                assert_eq!(complement_union, intersect_complement);
                assert_eq!(complement_intersect, union_complement);

                let union_rev = s.union(spp2, spp1);
                let intersect_rev = s.intersect(spp2, spp1);
                assert_eq!(union, union_rev);
                assert_eq!(intersect, intersect_rev);

                // flip of sequence is sequence of flipped
                let seq = s.sequence(spp1, spp2);
                let seq_flip = s.flip(seq);
                let spp1_flip = s.flip(spp1);
                let spp2_flip = s.flip(spp2);
                // Note: seq_flip = flip(spp1; spp2)
                // Note: flip_seq = flip(spp2); flip(spp1)
                let flip_seq = s.sequence(spp2_flip, spp1_flip);
                assert_eq!(seq_flip, flip_seq);
            }
        }
    }

    #[test]
    fn test_is_zero() {
        let mut s = SPPstore::new(N);
        
        // Test base cases
        assert!(s.is_zero(s.zero));
        assert!(!s.is_zero(s.one));
        assert!(!s.is_zero(s.top));
        
        // Test that zero built at any depth is detected as zero
        let mut zero_depth_2 = SPP::new(0);
        for _ in 0..2 {
            zero_depth_2 = s.mk(zero_depth_2, zero_depth_2, zero_depth_2, zero_depth_2);
        }
        assert!(s.is_zero(zero_depth_2));
        
        // Test a non-zero SPP
        let non_zero = s.mk(s.zero, s.one, s.zero, s.zero);
        assert!(!s.is_zero(non_zero));
        
        // Test all SPPs
        let all = s.all();
        for spp in all {
            let is_zero_result = s.is_zero(spp);
            // An SPP is zero if it equals the zero SPP
            assert_eq!(is_zero_result, spp == s.zero);
        }
    }
}

#[cfg(test)]
mod image_allocation_tests {
    use super::*;

    #[test]
    fn images_and_projections_create_no_relation_nodes() {
        let mut m = SPPstore::new(12);
        let assign = m.assign(3, true);
        let test = m.test(7, false);
        let relation = m.union(assign, test);
        let input = m.sp.test(0, true);
        let before = m.nodes.len();
        m.push(input, relation);
        m.pull(relation, input);
        m.fwd(relation);
        m.bwd(relation);
        m.has_image(input, relation);
        assert_eq!(m.nodes.len(), before);
    }
}
