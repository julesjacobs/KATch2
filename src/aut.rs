use crate::expr::Expr;
use crate::spp;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
// An AExpr represents an automaton state.
// This is essentially a compressed and hash-consed form of a NetKAT expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum AExpr {
    SPP(spp::SPP), // We keep field tests and mutations and combinations thereof in SPP form
    Union(Vec<State>), // e1 + e2 + ... + en
    Intersect(Vec<State>), // e1 & e2 & ... & en
    Xor(State, State), // e1 ^ e2
    Difference(State, State), // e1 - e2
    Complement(State), // !e1
    Sequence(State, State), // e1; e2
    Star(State),   // e*
    Dup,           // dup
    LtlNext(State), // X e
    LtlUntil(State, State), // e1 U e2
    Top,           // represents the set of all strings
}

// A State is an index into the Aut's expression table.
type State = usize;

/// Symbolic transitions ST<T>.           
/// Symbolic transitions represent, for each T, a set of packet pairs that can transition to T. These are represented as a finite map from T to SPP's.
/// A symbolic transition can be deterministic or nondeterministic, depending on whether the SPPs associated with different T's are disjoint. We typically keep ST's in deterministic form.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ST {
    transitions: HashMap<State, spp::SPP>,
}

impl ST {
    pub fn new(transitions: HashMap<State, spp::SPP>) -> Self {
        ST { transitions }
    }
    pub fn empty() -> Self {
        ST {
            transitions: HashMap::new(),
        }
    }

    /// Returns a reference to the internal transitions map
    pub fn get_transitions(&self) -> &HashMap<State, spp::SPP> {
        &self.transitions
    }
}

pub struct Aut {
    aexprs: Vec<AExpr>,
    aexpr_map: HashMap<AExpr, State>,
    delta_map: HashMap<State, ST>,
    epsilon_map: HashMap<State, spp::SPP>,
    spp: spp::SPPstore,
    num_calls: u32,
}

impl Aut {
    pub fn new(num_vars: u32) -> Self {
        let aut = Aut {
            aexprs: vec![],
            aexpr_map: HashMap::new(),
            delta_map: HashMap::new(),
            epsilon_map: HashMap::new(),
            spp: spp::SPPstore::new(num_vars),
            num_calls: 0,
        };
        aut
    }

    // --- States ---

    // Internal function to hash-cons an expression
    fn intern(&mut self, expr: AExpr) -> State {
        if let Some(&id) = self.aexpr_map.get(&expr) {
            return id;
        }
        let id = self.aexprs.len();
        self.aexprs.push(expr.clone());
        self.aexpr_map.insert(expr, id);
        id
    }

    // Smart Constructors with Simplifications

    fn mk_spp(&mut self, spp: spp::SPP) -> State {
        // TODO: Add simplification for SPP constants (0, 1) if not handled by SPPstore itself
        self.intern(AExpr::SPP(spp))
    }

    fn mk_union_n(&mut self, states: Vec<State>) -> State {
        let mut states2 = vec![];
        for state in states {
            match self.get_expr(state) {
                AExpr::Union(nested) => {
                    for nested_state in nested.clone() {
                        states2.push(nested_state);
                    }
                }
                AExpr::Top => {
                    return self.mk_top();
                }
                _ => states2.push(state),
            }
        }
        let mut spp = self.spp.zero;
        let mut new_states = vec![];
        for state in states2.clone() {
            match self.get_expr(state) {
                AExpr::SPP(s) => spp = self.spp.union(spp, *s),
                _ => new_states.push(state),
            }
        }
        if spp != self.spp.zero {
            new_states.push(self.mk_spp(spp));
        }

        new_states.sort();
        new_states.dedup();
        // Special case: just one element
        if new_states.len() == 1 {
            return new_states[0];
        }
        if new_states.is_empty() {
            return self.mk_spp(self.spp.zero);
        }

        // Create an n-ary union
        self.intern(AExpr::Union(new_states))
    }

    fn mk_union(&mut self, e1: State, e2: State) -> State {
        self.mk_union_n(vec![e1, e2])
    }

    fn mk_intersect_n(&mut self, states: Vec<State>) -> State {
        let mut new_states = vec![];
        for state in states {
            match self.get_expr(state) {
                AExpr::Intersect(nested) => {
                    for nested_state in nested.clone() {
                        new_states.push(nested_state);
                    }
                }
                _ => new_states.push(state),
            }
        }

        // Distribute intersections over unions
        let mut distributed_states: Vec<Vec<State>> = vec![vec![]];
        for state in new_states {
            match self.get_expr(state) {
                AExpr::Union(nested) => {
                    let mut new_distributed_states = vec![];
                    for nested_state in nested.clone() {
                        for i in 0..distributed_states.len() {
                            let mut new_distributed_state = distributed_states[i].clone();
                            new_distributed_state.push(nested_state);
                            new_distributed_states.push(new_distributed_state);
                        }
                    }
                    distributed_states = new_distributed_states;
                }
                _ => {
                    for i in 0..distributed_states.len() {
                        distributed_states[i].push(state);
                    }
                }
            }
        }

        // Make an actual intersection of each distributed state
        let mut intersections = vec![];
        for distributed_state in distributed_states {
            intersections.push(self.mk_intersect_n_base(distributed_state));
        }

        return self.mk_union_n(intersections);
    }

    fn mk_intersect_n_base(&mut self, mut states: Vec<State>) -> State {
        // Basic version that does not do distribution, but does handle Top and Zero and merges SPPs
        let mut new_states = vec![];
        for state in states {
            match self.get_expr(state) {
                AExpr::Intersect(nested) => {
                    for nested_state in nested.clone() {
                        new_states.push(nested_state);
                    }
                }
                _ => new_states.push(state),
            }
        }
        states = new_states;
        // Remove Top
        states.retain(|&state| state != self.mk_top());
        // Intersect the SPPs, collecting the rest
        let mut spp = None;
        let mut rest = vec![];
        for state in states {
            match self.get_expr(state) {
                AExpr::SPP(s) => spp = Some(self.spp.intersect(spp.unwrap_or(self.spp.top), *s)),
                _ => rest.push(state),
            }
        }
        rest.sort();
        rest.dedup();
        if spp == Some(self.spp.zero) {
            return self.mk_spp(self.spp.zero);
        }
        if rest.is_empty() {
            if let Some(spp) = spp {
                return self.mk_spp(spp);
            } else {
                return self.mk_top();
            }
        }
        if let Some(spp) = spp {
            rest.push(self.mk_spp(spp));
        }
        self.intern(AExpr::Intersect(rest))
    }

    fn mk_intersect(&mut self, e1: State, e2: State) -> State {
        self.mk_intersect_n(vec![e1, e2])
    }

    fn mk_xor(&mut self, e1: State, e2: State) -> State {
        if e1 == e2 {
            return self.mk_spp(self.spp.zero);
        } // e ^ e = 0

        // SPP Simplification: s1 ^ s2
        if let (AExpr::SPP(s1), AExpr::SPP(s2)) = (self.get_expr(e1), self.get_expr(e2)) {
            let result_spp = self.spp.xor(*s1, *s2);
            return self.mk_spp(result_spp);
        }

        // Canonical ordering
        let (e1, e2) = if e1 < e2 { (e1, e2) } else { (e2, e1) };
        self.intern(AExpr::Xor(e1, e2))
    }

    fn mk_difference(&mut self, e1: State, e2: State) -> State {
        if e1 == e2 {
            return self.mk_spp(self.spp.zero);
        } // e - e = 0

        // SPP Simplification: s1 - s2
        if let (AExpr::SPP(s1), AExpr::SPP(s2)) = (self.get_expr(e1), self.get_expr(e2)) {
            let result_spp = self.spp.difference(*s1, *s2);
            return self.mk_spp(result_spp);
        }

        self.intern(AExpr::Difference(e1, e2))
    }

    fn mk_complement(&mut self, e: State) -> State {
        // Simplify !!e = e
        if let AExpr::Complement(inner_e) = self.get_expr(e) {
            return *inner_e;
        }

        // // De Morgan's laws and complement simplifications
        let expr = self.get_expr(e).clone();
        // match expr {
        //     AExpr::Union(states) => {
        //         // !(e1 + e2 + ... + en) = !e1 & !e2 & ... & !en
        //         let complements: Vec<State> = states
        //             .iter()
        //             .map(|&state| self.mk_complement(state))
        //             .collect();
        //         return self.mk_intersect_n(complements);
        //     }
        //     AExpr::Intersect(states) => {
        //         // !(e1 & e2 & ... & en) = !e1 + !e2 + ... + !en
        //         let complements: Vec<State> = states
        //             .iter()
        //             .map(|&state| self.mk_complement(state))
        //             .collect();
        //         return self.mk_union_n(complements);
        //     }
        //     AExpr::Difference(e1, e2) => {
        //         let c1 = self.mk_complement(e1);
        //         return self.mk_union(c1, e2);
        //     }
        //     AExpr::Xor(e1, e2) => {
        //         let c1 = self.mk_complement(e1);
        //         return self.mk_xor(c1, e2);
        //     }
        //     _ => {}
        // }

        // Simplify !⊤ = 0
        if let AExpr::Top = expr {
            return self.mk_spp(self.spp.zero);
        }

        // SPP Simplification is not valid here, since we are complementing a set of strings!
        self.intern(AExpr::Complement(e))
    }

    fn mk_sequence(&mut self, e1: State, e2: State) -> State {
        // SPP Simplification: s1 ; s2
        if let (AExpr::SPP(s1), AExpr::SPP(s2)) = (self.get_expr(e1), self.get_expr(e2)) {
            let result_spp = self.spp.sequence(*s1, *s2);
            return self.mk_spp(result_spp);
        }
        // Simplify SPP.one ; e = e and e ; SPP.one = e and SPP.zero ; e = SPP.zero and e ; SPP.zero = SPP.zero
        match self.get_expr(e1) {
            AExpr::SPP(s1) => {
                if *s1 == self.spp.one {
                    return e2;
                }
                if *s1 == self.spp.zero {
                    return self.mk_spp(self.spp.zero);
                }
            }
            _ => {}
        }
        match self.get_expr(e2) {
            AExpr::SPP(s2) => {
                if *s2 == self.spp.one {
                    return e1;
                }
                if *s2 == self.spp.zero {
                    return self.mk_spp(self.spp.zero);
                }
            }
            _ => {}
        }

        // (a; b); c = a; (b; c)
        if let AExpr::Sequence(e11, e12) = self.get_expr(e1).clone() {
            let tmp = self.mk_sequence(e12, e2);
            return self.mk_sequence(e11, tmp);
        }

        // Simplify T; T = T
        if let (AExpr::Top, AExpr::Top) = (self.get_expr(e1), self.get_expr(e2)) {
            return self.mk_top();
        }

        self.intern(AExpr::Sequence(e1, e2))
    }

    fn mk_star(&mut self, e: State) -> State {
        // Simplify (e*)* = e*
        if let AExpr::Star(_inner_e) = self.get_expr(e) {
            // Return the existing e* index
            return e;
        }

        // SPP Simplification: s*
        if let AExpr::SPP(s) = self.get_expr(e) {
            let result_spp = self.spp.star(*s);
            return self.mk_spp(result_spp);
        }

        // Simplify T* = T
        if let AExpr::Top = self.get_expr(e) {
            return self.mk_top();
        }

        self.intern(AExpr::Star(e))
    }

    fn mk_dup(&mut self) -> State {
        self.intern(AExpr::Dup)
    }

    fn mk_top(&mut self) -> State {
        self.intern(AExpr::Top)
    }

    fn mk_until(&mut self, e1: State, e2: State) -> State {
        self.intern(AExpr::LtlUntil(e1, e2))
    }

    // Helper to get the actual expression from an index
    fn get_expr(&self, id: State) -> &AExpr {
        &self.aexprs[id]
    }

    // Function to convert an external Expr to an internal AExp index
    pub fn expr_to_state(&mut self, expr: &Expr) -> State {
        match expr {
            Expr::Zero => self.mk_spp(self.spp.zero),
            Expr::One => self.mk_spp(self.spp.one),
            Expr::Top => self.mk_top(),
            Expr::Assign(field, value) => {
                let spp = self.spp.assign(*field, *value);
                self.mk_spp(spp)
            }
            Expr::Test(field, value) => {
                let spp = self.spp.test(*field, *value);
                self.mk_spp(spp)
            }
            Expr::Union(e1, e2) => {
                let aexp1 = self.expr_to_state(e1); // e1 is Box<Expr>, dereferences automatically
                let aexp2 = self.expr_to_state(e2);
                self.mk_union(aexp1, aexp2)
            }
            Expr::Intersect(e1, e2) => {
                let aexp1 = self.expr_to_state(e1);
                let aexp2 = self.expr_to_state(e2);
                self.mk_intersect(aexp1, aexp2)
            }
            Expr::Xor(e1, e2) => {
                let aexp1 = self.expr_to_state(e1);
                let aexp2 = self.expr_to_state(e2);
                self.mk_xor(aexp1, aexp2)
            }
            Expr::Difference(e1, e2) => {
                let aexp1 = self.expr_to_state(e1);
                let aexp2 = self.expr_to_state(e2);
                self.mk_difference(aexp1, aexp2)
            }
            Expr::Complement(e) => {
                let aexp = self.expr_to_state(e);
                self.mk_complement(aexp)
            }
            Expr::Sequence(e1, e2) => {
                let aexp1 = self.expr_to_state(e1);
                let aexp2 = self.expr_to_state(e2);
                self.mk_sequence(aexp1, aexp2)
            }
            Expr::Star(e) => {
                let aexp = self.expr_to_state(e);
                self.mk_star(aexp)
            }
            Expr::Dup => self.mk_dup(),
            Expr::LtlNext(e) => {
                let aexp = self.expr_to_state(e);
                self.intern(AExpr::LtlNext(aexp))
            }
            Expr::LtlUntil(e1, e2) => {
                let aexp1 = self.expr_to_state(e1);
                let aexp2 = self.expr_to_state(e2);
                self.intern(AExpr::LtlUntil(aexp1, aexp2))
            }
            Expr::End => self.mk_spp(self.spp.top),
        }
    }

    // --- Symbolic transitions: ST ---

    /// The empty ST
    pub fn st_empty(&mut self) -> ST {
        ST::new(HashMap::new())
    }

    /// Creates a singleton ST mapping `spp` to `state`
    pub fn st_singleton(&mut self, spp: spp::SPP, state: State) -> ST {
        if spp == self.spp.zero {
            return ST::new(HashMap::new());
        }
        if state == self.mk_spp(self.spp.zero) {
            return ST::new(HashMap::new());
        }
        ST::new(HashMap::from([(state, spp)]))
    }

    // /// Insert a transition into a ST.
    // /// Precondition: spp is disjoint from all other spp's in the ST
    // pub fn st_insert_unsafe(&mut self, st: &mut ST, state: State, spp: spp::SPP) {
    //     // Assert that spp is disjoint from all other spp's in the ST
    //     #[cfg(debug_assertions)]
    //     for (_, existing_spp) in st.transitions.iter() {
    //         debug_assert!(
    //             self.spp.intersect(*existing_spp, spp) == self.spp.zero,
    //             "spp must be disjoint from all other spp's in the ST"
    //         );
    //     }
    //     // Check if the state already exists, if so union the spp's
    //     if let Some(existing_spp) = st.transitions.get_mut(&state) {
    //         *existing_spp = self.spp.union(*existing_spp, spp);
    //     } else {
    //         // Check if the spp is 0, if so don't insert
    //         if spp == self.spp.zero {
    //             return;
    //         }
    //         // Check if the State is zero, if so don't insert
    //         if state == self.mk_spp(self.spp.zero) {
    //             return;
    //         }
    //         st.transitions.insert(state, spp);
    //     }
    // }

    fn st_insert_helper(&mut self, st: &mut ST, state: State, spp: spp::SPP) {
        if spp == self.spp.zero {
            return;
        }
        if state == self.mk_spp(self.spp.zero) {
            return;
        }
        if let Some(existing_spp) = st.transitions.get_mut(&state) {
            *existing_spp = self.spp.union(*existing_spp, spp);
        } else {
            st.transitions.insert(state, spp);
        }
    }

    /// Insert a transition into a ST.
    pub fn st_insert(&mut self, st: &mut ST, state: State, spp: spp::SPP) {
        // We have to be careful here because a naive implementation would not result in a deterministic ST
        // Strategy: intersect the spp with all other spp's in the ST, and insert an expr union for those
        // Separately keep track of the remaining spp that is inserted separately

        let mut result = ST::empty();
        let mut total_instersect = self.spp.zero;
        for (state2, spp2) in st.transitions.clone() {
            let intersect_spp = self.spp.intersect(spp, spp2);
            let union_state = self.mk_union(state, state2);
            self.st_insert_helper(&mut result, union_state, intersect_spp);
            total_instersect = self.spp.union(total_instersect, intersect_spp);
            let diff_spp = self.spp.difference(spp2, intersect_spp);
            self.st_insert_helper(&mut result, state2, diff_spp);
        }

        let remaining_spp = self.spp.difference(spp, total_instersect);
        self.st_insert_helper(&mut result, state, remaining_spp);
        *st = result;
    }

    fn st_intersect(&mut self, st1: ST, st2: ST) -> ST {
        let mut result = ST::empty();
        for (state1, spp1) in &st1.transitions {
            for (state2, spp2) in &st2.transitions {
                let intersect_state = self.mk_intersect(*state1, *state2);
                let spp = self.spp.intersect(*spp1, *spp2);
                self.st_insert(&mut result, intersect_state, spp);
            }
        }
        result
    }

    pub fn st_union(&mut self, st1: ST, st2: ST) -> ST {
        let mut result = ST::empty();
        for (state, spp) in st1.transitions {
            self.st_insert(&mut result, state, spp);
        }
        for (state, spp) in st2.transitions {
            self.st_insert(&mut result, state, spp);
        }
        result
    }

    fn st_difference(&mut self, st1: ST, st2: ST) -> ST {
        // (st1 - st2) = st1 & !st2
        // let st2_complement = self.st_complement(st2);
        // self.st_intersect(st1, st2_complement)
        let mut result = ST::empty();
        let mut spp_sum = self.spp.zero;
        for (state1, spp1) in st1.transitions.clone() {
            for (state2, spp2) in st2.transitions.clone() {
                let diff_state = self.mk_difference(state1, state2);
                let inter_spp = self.spp.intersect(spp1, spp2);
                spp_sum = self.spp.union(spp_sum, inter_spp);
                self.st_insert(&mut result, diff_state, inter_spp);
            }
        }
        for (state, spp) in st1.transitions.clone() {
            let diff_spp = self.spp.difference(spp, spp_sum);
            self.st_insert(&mut result, state, diff_spp);
        }
        result
    }

    fn st_xor(&mut self, st1: ST, st2: ST) -> ST {
        // (st1 ^ st2) = (st1 - st2) + (st2 - st1)
        let st1_minus_st2 = self.st_difference(st1.clone(), st2.clone());
        let st2_minus_st1 = self.st_difference(st2, st1);
        self.st_union(st1_minus_st2, st2_minus_st1)
    }

    fn st_complement(&mut self, st: ST) -> ST {
        let mut result = ST::empty();
        for (&state, &spp) in &st.transitions {
            let new_state = self.mk_complement(state);
            self.st_insert(&mut result, new_state, spp);
        }
        // Find the union of all the spp's in the transitions
        let mut union_spp = self.spp.zero;
        for (_, &spp) in &st.transitions {
            union_spp = self.spp.union(union_spp, spp);
        }
        // Add a transition from the complement of the union to the Top state
        let complement_spp = self.spp.complement(union_spp);
        let top = self.mk_top();
        self.st_insert(&mut result, top, complement_spp);
        result
    }

    fn st_postcompose(&mut self, st: ST, expr: State) -> ST {
        let mut result = ST::empty();
        for (state, spp) in st.transitions {
            let new_state = self.mk_sequence(state, expr);
            self.st_insert(&mut result, new_state, spp);
        }
        result
    }

    fn st_precompose(&mut self, spp: spp::SPP, st: ST) -> ST {
        // Here we have to be careful because a naive implementation would not result in a deterministic ST
        // Therefore we use st_insert and not st_insert_unsafe
        let mut result = ST::empty();
        for (state, spp2) in st.transitions {
            let new_spp = self.spp.sequence(spp, spp2);
            self.st_insert(&mut result, state, new_spp);
        }
        result
    }

    /// Helper function for intersecting an ST with an `expr` on the right
    /// (used for computing the derivative of `e1 U e2`)
    fn st_intersect_expr(&mut self, st: ST, expr: State) -> ST {
        let mut result = ST::empty();
        for (state, spp) in st.transitions {
            let new_state = self.mk_intersect(state, expr);
            self.st_insert(&mut result, new_state, spp);
        }
        result
    }

    // --- Automaton construction: delta, epsilon ---

    pub fn delta(&mut self, state: State) -> ST {
        self.num_calls += 1;
        if self.num_calls > 100000 {
            panic!(
                "Delta called {} times, artificial limit reached for state {}",
                self.num_calls,
                self.state_to_string(state)
            );
        }
        if self.state_to_string(state).len() > 1000000 {
            panic!(
                "Delta called with state length = {}: {}",
                self.state_to_string(state).len(),
                self.state_to_string(state)
            );
        }
        if let Some(st) = self.delta_map.get(&state) {
            return st.clone();
        }

        // Extract all needed information from the expr before recursive calls
        let expr = self.get_expr(state).clone();

        // Calculate delta for each case
        let result = match expr {
            AExpr::SPP(_) => ST::empty(),
            AExpr::Union(states) => {
                let states_copy = states.clone();
                let mut result = ST::empty();
                for s in states_copy {
                    let delta_state = self.delta(s);
                    result = self.st_union(result, delta_state);
                }
                result
            }
            AExpr::Intersect(states) => {
                // Compute all delta values first to avoid borrow issues
                let delta_values: Vec<ST> = states.iter().map(|&s| self.delta(s)).collect();

                // Then combine them with intersection
                if delta_values.is_empty() {
                    // Empty intersection is ST mapping all states to top (opposite of union's empty case)
                    // In practice, we don't expect to hit this case
                    ST::empty()
                } else {
                    // Combine using intersection
                    let mut result = delta_values[0].clone();
                    for delta in &delta_values[1..] {
                        result = self.st_intersect(result, delta.clone());
                    }
                    result
                }
            }
            AExpr::Xor(e1, e2) => {
                let delta1 = self.delta(e1);
                let delta2 = self.delta(e2);
                self.st_xor(delta1, delta2)
            }
            AExpr::Difference(e1, e2) => {
                let delta1 = self.delta(e1);
                let delta2 = self.delta(e2);
                self.st_difference(delta1, delta2)
            }
            AExpr::Complement(e) => {
                let delta_e = self.delta(e);
                self.st_complement(delta_e)
            }
            AExpr::Sequence(e1, e2) => {
                // delta(e1 e2) = delta(e1) e2 + epsilon(e1) delta(e2)
                let epsilon_e1 = self.epsilon(e1);
                let delta_e1 = self.delta(e1);
                let delta_e2 = self.delta(e2);
                let delta_e1_seq_e2 = self.st_postcompose(delta_e1, e2);
                let epsilon_e1_seq_e2 = self.st_precompose(epsilon_e1, delta_e2);
                self.st_union(delta_e1_seq_e2, epsilon_e1_seq_e2)
            }
            AExpr::Star(e) => {
                // delta(e*) = epsilon(e)* delta(e) e*
                let epsilon_e = self.epsilon(e);
                let epsilon_e_star = self.spp.star(epsilon_e);
                let delta_e = self.delta(e);
                let delta_e_star_e = self.st_postcompose(delta_e, state);
                self.st_precompose(epsilon_e_star, delta_e_star_e)
            }
            AExpr::Dup => {
                let spp_one = self.mk_spp(self.spp.one);
                self.st_singleton(self.spp.one, spp_one)
            }
            AExpr::LtlNext(e) => self.st_singleton(self.spp.top, e),
            AExpr::LtlUntil(e1, e2) => {
                // delta(e1 U e2) = delta(e2) ∪ (delta(e1) ∩ (e1 U e2))
                let delta_e1 = self.delta(e1);
                let delta_e2 = self.delta(e2);
                let e1_u_e2 = self.mk_until(e1, e2);
                let delta_e1_intersect_e1_u_e2 = self.st_intersect_expr(delta_e1, e1_u_e2);
                self.st_union(delta_e2, delta_e1_intersect_e1_u_e2)
            }
            AExpr::Top => {
                let top = self.mk_top();
                self.st_singleton(self.spp.top, top)
            }
        };

        // Cache the result
        self.delta_map.insert(state, result.clone());
        result
    }

    pub fn epsilon(&mut self, state: State) -> spp::SPP {
        // Check if we've already calculated this
        if let Some(&spp) = self.epsilon_map.get(&state) {
            return spp;
        }

        // Clone the expression to avoid borrowing issues
        let expr = self.get_expr(state).clone();

        // Calculate epsilon for each case
        let result = match expr {
            AExpr::SPP(spp) => spp,
            AExpr::Union(states) => {
                // Pre-compute all epsilon values
                let epsilon_values: Vec<spp::SPP> =
                    states.iter().map(|&s| self.epsilon(s)).collect();

                // Then combine them
                let mut result = self.spp.zero;
                for &eps in &epsilon_values {
                    result = self.spp.union(result, eps);
                }
                result
            }
            AExpr::Intersect(states) => {
                // Pre-compute all epsilon values
                let epsilon_values: Vec<spp::SPP> =
                    states.iter().map(|&s| self.epsilon(s)).collect();

                // Then combine them
                if epsilon_values.is_empty() {
                    self.spp.one
                } else {
                    let mut result = epsilon_values[0];
                    for &eps in &epsilon_values[1..] {
                        result = self.spp.intersect(result, eps);
                    }
                    result
                }
            }
            AExpr::Xor(e1, e2) => {
                let eps1 = self.epsilon(e1);
                let eps2 = self.epsilon(e2);
                self.spp.xor(eps1, eps2)
            }
            AExpr::Difference(e1, e2) => {
                let eps1 = self.epsilon(e1);
                let eps2 = self.epsilon(e2);
                self.spp.difference(eps1, eps2)
            }
            AExpr::Complement(e) => {
                let eps = self.epsilon(e);
                self.spp.complement(eps)
            }
            AExpr::Sequence(e1, e2) => {
                let eps1 = self.epsilon(e1);
                let eps2 = self.epsilon(e2);
                self.spp.sequence(eps1, eps2)
            }
            AExpr::Star(e) => {
                let eps = self.epsilon(e);
                self.spp.star(eps)
            }
            AExpr::Dup => self.spp.zero,
            AExpr::LtlNext(_) => self.spp.zero,
            AExpr::LtlUntil(_e1, e2) => self.epsilon(e2),
            AExpr::Top => self.spp.top,
        };

        // Cache the result
        self.epsilon_map.insert(state, result);
        result
    }

    /// Returns a reference to the internal SPPstore
    pub fn spp_store(&self) -> &spp::SPPstore {
        &self.spp
    }

    /// Checks if the given state is empty
    pub fn is_empty(&mut self, state: State) -> bool {
        // Todo: list of states to visit
        // Note: One = Top for SPs
        let mut todo = vec![(state, self.spp.sp.one)];
        // Hashmap of SPs for each state reachable from the given state
        let mut sp_map = HashMap::new();
        while !todo.is_empty() {
            let (state, sp) = todo.pop().unwrap();
            // Union the SPP into the map
            let original_sp = sp_map.entry(state).or_insert(self.spp.sp.zero);
            let to_add = self.spp.sp.difference(sp, *original_sp);
            if to_add != self.spp.sp.zero {
                *original_sp = self.spp.sp.union(*original_sp, to_add);
                // iterate over all transitions from the state
                for (state2, spp2) in self.delta(state).transitions {
                    // NB: `push(to_add, spp2)   naive_forward(to_add; spp2)`,
                    // where `;` is sequential composition
                    let seq_forward = self.spp.push(to_add, spp2);
                    todo.push((state2, seq_forward));
                }
            }
        }

        // Check if the SPPs in the map when composed with the epsilon of the given state are empty
        for (state, sp) in sp_map {
            let epsilon_spp: spp::SPP = self.epsilon(state);
            let sp_composed = self.spp.push(sp, epsilon_spp);
            if sp_composed != self.spp.sp.zero {
                return false;
            }
        }
        true
    }

    /// Computes a packet transformer for the given state (equivalent to eliminating dup)
    pub fn eliminate_dup(&mut self, state: State) -> spp::SPP {
        // We implement this using Kleene's algorithm
        // We start by using the delta function to explore the automaton forward from this state
        // And store the automaton's SPP edges we find
        // We add an additional end state and from each state add the transition epsilon(state) to the end state
        // We then use Kleene's algorithm to eliminate all states except the end state
        // The self loop stored at the end state is the packet transformer SPP that we are looking for
        todo!()
    }


    /// Returns a string representation of the AExpr for the given state
    pub fn state_to_string(&self, state: State) -> String {
        match self.get_expr(state) {
            AExpr::SPP(spp) => format!("SPP({})", spp),
            AExpr::Union(states) => {
                let state_strings: Vec<String> =
                    states.iter().map(|&s| self.state_to_string(s)).collect();
                format!("({})", state_strings.join(" + "))
            }
            AExpr::Intersect(states) => {
                let state_strings: Vec<String> =
                    states.iter().map(|&s| self.state_to_string(s)).collect();
                format!("({})", state_strings.join(" & "))
            }
            AExpr::Xor(e1, e2) => format!(
                "({} ^ {})",
                self.state_to_string(*e1),
                self.state_to_string(*e2)
            ),
            AExpr::Difference(e1, e2) => format!(
                "({} - {})",
                self.state_to_string(*e1),
                self.state_to_string(*e2)
            ),
            AExpr::Complement(e) => format!("!{}", self.state_to_string(*e)),
            AExpr::Sequence(e1, e2) => format!(
                "({} ; {})",
                self.state_to_string(*e1),
                self.state_to_string(*e2)
            ),
            AExpr::Star(e) => format!("({})*", self.state_to_string(*e)),
            AExpr::Dup => "dup".to_string(),
            AExpr::LtlNext(e) => format!("X({})", self.state_to_string(*e)),
            AExpr::LtlUntil(e1, e2) => format!(
                "({} U {})",
                self.state_to_string(*e1),
                self.state_to_string(*e2)
            ),
            AExpr::Top => "⊤".to_string(),
        }
    }

    /// Collects all SPP indices from the expression at the given state
    pub fn collect_spps(&self, state: State, spps: &mut HashSet<spp::SPP>) {
        match self.get_expr(state) {
            AExpr::SPP(spp) => {
                spps.insert(*spp);
            }
            AExpr::Union(states) => {
                for &s in states {
                    self.collect_spps(s, spps);
                }
            }
            AExpr::Intersect(states) => {
                for &s in states {
                    self.collect_spps(s, spps);
                }
            }
            AExpr::Xor(e1, e2)
            | AExpr::Difference(e1, e2)
            | AExpr::Sequence(e1, e2)
            | AExpr::LtlUntil(e1, e2) => {
                self.collect_spps(*e1, spps);
                self.collect_spps(*e2, spps);
            }
            AExpr::Complement(e) | AExpr::Star(e) | AExpr::LtlNext(e) => {
                self.collect_spps(*e, spps);
            }
            AExpr::Dup | AExpr::Top => {}
        }
    }
}
