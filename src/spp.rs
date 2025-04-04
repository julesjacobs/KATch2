// Symbolic Packet Programs represent relations of concrete packets.
// They are represented in a BDD-like structure.
// Unlike traditional BDDs, we do not leave out any levels of the BDD:
// each path down the BDD has precisely the same depth, namely the number of variables, i.e. the packet size in bits.

#[allow(non_snake_case)]
use std::collections::HashMap;

/// We use indices into the SPP store to represent SPPs.
/// The zero SPP is represented by 0 and the one SPP is represented by 1.
/// Indices into the store are the u32 value - 2.
pub type SPP = u32;
pub type Var = u32;

/// The store of SPs.
#[derive(Debug)]
pub struct SPPstore {
    num_vars: Var, // Idea: it's ok to pick this larger than you need. Hash consing & memoization will handle it
    nodes: Vec<SPPnode>,
    hc: HashMap<SPPnode, SPP>,
    zero: SPP,
    one: SPP,
    top: SPP,

    // Memo tables for the operations
    union_memo: HashMap<(SPP, SPP), SPP>,
    intersect_memo: HashMap<(SPP, SPP), SPP>,
    sequence_memo: HashMap<(SPP, SPP), SPP>,
    star_memo: HashMap<SPP, SPP>,
    complement_memo: HashMap<SPP, SPP>,
    ifelse_memo: HashMap<(Var, SPP, SPP), SPP>,
}

/// A node in the SPP store. Has four children, one for each combination of the two variables.
#[derive(Debug, Eq, Hash, PartialEq, Clone, Copy)]
struct SPPnode {
    x00: SPP,
    x01: SPP,
    x10: SPP,
    x11: SPP,
}

impl SPPstore {
    pub fn new(num_vars: Var) -> Self {
        let mut store = Self {
            num_vars,
            nodes: vec![],
            hc: HashMap::new(),
            zero: 0,
            one: 0,
            top: 0, // Dummy values, will be set later
            // We prefill the memo tables with the results of the trivial cases
            // Need to benchmark if this is actually faster than checking these cases in the operations
            union_memo: HashMap::from([((0, 0), 0), ((0, 1), 1), ((1, 0), 1), ((1, 1), 1)]),
            intersect_memo: HashMap::from([((0, 0), 0), ((0, 1), 0), ((1, 0), 0), ((1, 1), 1)]),
            sequence_memo: HashMap::from([((0, 0), 0), ((0, 1), 0), ((1, 0), 0), ((1, 1), 1)]),
            star_memo: HashMap::from([(0, 1), (1, 1)]),
            complement_memo: HashMap::from([(0, 1), (1, 0)]),
            ifelse_memo: HashMap::new(),
        };
        store.zero = store.zero();
        store.one = store.one();
        store.top = store.top();
        store
    }

    fn get(&self, spp: SPP) -> SPPnode {
        self.nodes[spp as usize - 2]
    }

    fn mk(&mut self, x00: SPP, x01: SPP, x10: SPP, x11: SPP) -> SPP {
        let node = SPPnode { x00, x01, x10, x11 };

        // Check if the node is already in the store using the hc table
        if let Some(spp) = self.hc.get(&node) {
            return *spp;
        }

        // Add the node to the store
        let spp = self.nodes.len() as SPP + 2;
        self.nodes.push(node);
        self.hc.insert(node, spp);
        spp
    }

    fn zero(&mut self) -> SPP {
        // We must construct a zero SPP of the right depth
        let mut spp = 0;
        for _ in 0..self.num_vars {
            spp = self.mk(spp, spp, spp, spp);
        }
        spp
    }
    fn top(&mut self) -> SPP {
        // We must construct a top SPP of the right depth
        let mut spp = 1;
        for _ in 0..self.num_vars {
            spp = self.mk(spp, spp, spp, spp);
        }
        spp
    }
    fn one(&mut self) -> SPP {
        // We must construct a one SPP of the right depth
        let mut spp_one = 1;
        let mut spp_zero = 0;
        for _ in 0..self.num_vars {
            spp_one = self.mk(spp_one, spp_zero, spp_zero, spp_one);
            spp_zero = self.mk(spp_zero, spp_zero, spp_zero, spp_zero);
        }
        spp_one
    }

    pub fn rand(&mut self) -> SPP {
        self.rand_helper(self.num_vars)
    }
    fn rand_helper(&mut self, depth: Var) -> SPP {
        if depth == 0 {
            return if rand::random::<bool>() { 0 } else { 1 };
        }
        let x00 = self.rand_helper(depth - 1);
        let x01 = self.rand_helper(depth - 1);
        let x10 = self.rand_helper(depth - 1);
        let x11 = self.rand_helper(depth - 1);
        self.mk(x00, x01, x10, x11)
    }

    pub fn union(&mut self, a: SPP, b: SPP) -> SPP {
        // First, check the memo table
        if let Some(&result) = self.union_memo.get(&(a, b)) {
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
        self.union_memo.insert((a, b), res);
        res
    }

    pub fn intersect(&mut self, a: SPP, b: SPP) -> SPP {
        // First, check the memo table
        if let Some(&result) = self.intersect_memo.get(&(a, b)) {
            return result;
        }
        // Because we prefilled the memo with base cases,
        // we now know that we've got a real node, so we don't need to handle 0 or 1 cases here.
        let a_node = self.get(a);
        let b_node = self.get(b);
        let x00 = self.intersect(a_node.x00, b_node.x00);
        let x01 = self.intersect(a_node.x01, b_node.x01);
        let x10 = self.intersect(a_node.x10, b_node.x10);
        let x11 = self.intersect(a_node.x11, b_node.x11);
        let res = self.mk(x00, x01, x10, x11);
        self.intersect_memo.insert((a, b), res);
        res
    }

    pub fn complement(&mut self, a: SPP) -> SPP {
        // First, check the memo table
        if let Some(&result) = self.complement_memo.get(&a) {
            return result;
        }
        // Because we prefilled the memo with base cases,
        // we now know that we've got a real node, so we don't need to handle 0 or 1 cases here.
        let node = self.get(a);
        let x00 = self.complement(node.x00);
        let x01 = self.complement(node.x01);
        let x10 = self.complement(node.x10);
        let x11 = self.complement(node.x11);
        let res = self.mk(x00, x01, x10, x11);
        self.complement_memo.insert(a, res);
        res
    }

    pub fn sequence(&mut self, a: SPP, b: SPP) -> SPP {
        // First, check the memo table
        if let Some(&result) = self.sequence_memo.get(&(a, b)) {
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
        self.sequence_memo.insert((a, b), res);
        res
    }

    pub fn star(&mut self, x: SPP) -> SPP {
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

    pub fn ifelse(&mut self, var: Var, then_branch: SPP, else_branch: SPP) -> SPP {
        assert!(var < self.num_vars);
        self.ifelse_helper(var, then_branch, else_branch)
    }
    fn ifelse_helper(&mut self, var: Var, then_branch: SPP, else_branch: SPP) -> SPP {
        // First, check the memo table
        if let Some(&result) = self.ifelse_memo.get(&(var, then_branch, else_branch)) {
            return result;
        }
        let then_node = self.get(then_branch);
        let else_node = self.get(else_branch);
        let x00;
        let x01;
        let x10;
        let x11;
        if var == 0 {
            x00 = then_node.x00;
            x01 = then_node.x01;
            x10 = else_node.x10;
            x11 = else_node.x11;
        } else {
            x00 = self.ifelse_helper(var - 1, then_node.x00, else_node.x00);
            x01 = self.ifelse_helper(var - 1, then_node.x01, else_node.x01);
            x10 = self.ifelse_helper(var - 1, then_node.x10, else_node.x10);
            x11 = self.ifelse_helper(var - 1, then_node.x11, else_node.x11);
        }
        let res = self.mk(x00, x01, x10, x11);
        self.ifelse_memo
            .insert((var, then_branch, else_branch), res);
        res
    }

    pub fn test(&mut self, var: Var, value: bool) -> SPP {
        // Implement in terms of ifelse
        if value {
            self.ifelse(var, self.one, self.zero)
        } else {
            self.ifelse(var, self.zero, self.one)
        }
    }

    pub fn all(&mut self) -> Vec<SPP> {
        return self.all_helper(self.num_vars);
    }
    pub fn all_helper(&mut self, depth: Var) -> Vec<SPP> {
        if depth == 0 {
            return vec![0, 1];
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
}

#[cfg(test)]
mod tests {
    use super::*;

    const N: Var = 1;

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
        let all = s.all();
        for &spp1 in &all {
            for &spp2 in &all {
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

                for i in 0..N {
                    let ifelse = s.ifelse(i, spp1, spp2);
                    let ifelse_complement = s.complement(ifelse);
                    assert_eq!(
                        ifelse_complement,
                        s.ifelse(i, spp1_complement, spp2_complement)
                    );
                }
            }
        }
    }
}
