// Symbolic Packets represent sets of concrete packets.
// They are represented in a BDD-like structure.
// Unlike traditional BDDs, we do not leave out any levels of the BDD:
// each path down the BDD has precisely the same depth, namely the number of variables, i.e. the packet size in bits.

use std::collections::HashMap;
use std::fmt;

/// We use indices into the SP store to represent SPs.
/// The zero SP is represented by SP(0) and the one SP is represented by SP(1).
/// Indices into the store are the u32 value - 2.
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

/// The store of SPs.
#[derive(Debug, Clone)]
pub struct SPstore {
    num_vars: Var, // Idea: it's ok to pick this larger than you need. Hash consing & memoization will handle it
    // Note: 0 & 1 don't appear in `hc` or the arena `nodes`, they only appear in
    // the other memo tables
    nodes: Vec<SPnode>,
    hc: HashMap<SPnode, SP>,
    pub zero: SP,
    pub one: SP,

    // Memo tables for the operations
    union_memo: HashMap<(SP, SP), SP>,
    intersect_memo: HashMap<(SP, SP), SP>,
    complement_memo: HashMap<SP, SP>,
    ifelse_memo: HashMap<(Var, SP, SP), SP>,
}

/// A node in the SP store. Has two children, one for this variable being 0 and one for it being 1.
/// An SPnode is a non-trivial SP (i.e. not zero and not one)
#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
pub struct SPnode {
    pub x0: SP,
    pub x1: SP,
}

impl SPstore {
    pub fn new(num_vars: Var) -> Self {
        let mut store = Self {
            num_vars,
            nodes: vec![],
            hc: HashMap::new(),
            zero: SP::new(0),
            one: SP::new(0), // Dummy values, will be set later
            // We prefill the memo tables with the results of the trivial cases
            // Need to benchmark if this is actually faster than checking these cases in the operations
            union_memo: HashMap::from([
                ((SP::new(0), SP::new(0)), SP::new(0)), 
                ((SP::new(0), SP::new(1)), SP::new(1)), 
                ((SP::new(1), SP::new(0)), SP::new(1)), 
                ((SP::new(1), SP::new(1)), SP::new(1))
            ]),
            intersect_memo: HashMap::from([
                ((SP::new(0), SP::new(0)), SP::new(0)), 
                ((SP::new(0), SP::new(1)), SP::new(0)), 
                ((SP::new(1), SP::new(0)), SP::new(0)), 
                ((SP::new(1), SP::new(1)), SP::new(1))
            ]),
            complement_memo: HashMap::from([
                (SP::new(0), SP::new(1)), 
                (SP::new(1), SP::new(0))
            ]),
            ifelse_memo: HashMap::new(),
        };
        store.zero = store.zero();
        store.one = store.one();
        store
    }

    pub fn get(&self, sp: SP) -> SPnode {
        self.nodes[sp.as_usize() - 2]
    }

    pub fn mk(&mut self, x0: SP, x1: SP) -> SP {
        let node = SPnode { x0, x1 };

        // Check if the node is already in the store using the hc table
        if let Some(sp) = self.hc.get(&node) {
            return *sp;
        }

        // Add the node to the store
        let sp = SP::new(self.nodes.len() as u32 + 2);
        self.nodes.push(node);
        self.hc.insert(node, sp);
        sp
    }

    fn zero(&mut self) -> SP {
        // We must construct a zero SP of the right depth
        let mut sp = SP::new(0);
        for _ in 0..self.num_vars {
            sp = self.mk(sp, sp);
        }
        sp
    }
    fn one(&mut self) -> SP {
        // We must construct a one SP of the right depth
        let mut sp = SP::new(1);
        for _ in 0..self.num_vars {
            sp = self.mk(sp, sp);
        }
        sp
    }

    pub fn rand(&mut self) -> SP {
        self.rand_helper(self.num_vars)
    }
    fn rand_helper(&mut self, depth: Var) -> SP {
        if depth == 0 {
            return if rand::random::<bool>() { SP::new(0) } else { SP::new(1) };
        }
        let x0 = self.rand_helper(depth - 1);
        let x1 = self.rand_helper(depth - 1);
        self.mk(x0, x1)
    }

    pub fn union(&mut self, a: SP, b: SP) -> SP {
        // First, check the memo table
        if let Some(&result) = self.union_memo.get(&(a, b)) {
            return result;
        }
        // We now know that we've got a real node, so we don't need to handle 0 or 1 cases here
        let a_node = self.get(a);
        let b_node = self.get(b);
        let x0 = self.union(a_node.x0, b_node.x0);
        let x1 = self.union(a_node.x1, b_node.x1);
        let res = self.mk(x0, x1);
        self.union_memo.insert((a, b), res);
        res
    }

    pub fn intersect(&mut self, a: SP, b: SP) -> SP {
        // First, check the memo table
        if let Some(&result) = self.intersect_memo.get(&(a, b)) {
            return result;
        }
        // Because we prefilled the memo with base cases,
        // we now know that we've got a real node, so we don't need to handle 0 or 1 cases here.
        let a_node = self.get(a);
        let b_node = self.get(b);
        let x0 = self.intersect(a_node.x0, b_node.x0);
        let x1 = self.intersect(a_node.x1, b_node.x1);
        let res = self.mk(x0, x1);
        self.intersect_memo.insert((a, b), res);
        res
    }

    pub fn complement(&mut self, a: SP) -> SP {
        // First, check the memo table
        if let Some(&result) = self.complement_memo.get(&a) {
            return result;
        }
        // Because we prefilled the memo with base cases,
        // we now know that we've got a real node, so we don't need to handle 0 or 1 cases here.
        let node = self.get(a);
        let x0 = self.complement(node.x0);
        let x1 = self.complement(node.x1);
        let res = self.mk(x0, x1);
        self.complement_memo.insert(a, res);
        res
    }

    /// Computes the difference of two SPPs using `sp1 - sp2 === sp1 & !sp2`
    pub fn difference(&mut self, sp1: SP, sp2: SP) -> SP {
        let not_sp2 = self.complement(sp2);
        self.intersect(sp1, not_sp2)
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
            x0 = then_node.x0;
            x1 = else_node.x1;
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
        // Implement in terms of ifelse
        if value {
            self.ifelse(var, self.one, self.zero)
        } else {
            self.ifelse(var, self.zero, self.one)
        }
    }

    pub fn all(&mut self) -> Vec<SP> {
        return self.all_helper(self.num_vars);
    }
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
}

#[cfg(test)]
mod tests {
    use super::*;

    const N: Var = 2;

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
}
