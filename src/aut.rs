use crate::expr::Expr;
use crate::sp;
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

// Symbolic transitions ST<T>
// Symbolic transitions represent, for each T, a set of packet pairs that can transition to T. These are represented as a finite map from T to SPP's.
// A symbolic transition can be deterministic or nondeterministic, depending on whether the SPPs associated with different T's are disjoint. We typically keep ST's in deterministic form.
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
    sp: sp::SPstore,
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
            sp: sp::SPstore::new(num_vars),
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

    /// Canonicalizes a list of states for union
    /// 1. Flattens nested unions
    /// 2. Sorts and removes duplicates
    /// 3. Combines all SPP children
    fn canonicalize_union(&mut self, mut states: Vec<State>) -> Vec<State> {
        // Keep track of SPPs to be combined
        let mut combined_spp = self.spp.zero;
        
        // Process all states to collect nested unions and SPPs
        let mut i = 0;
        while i < states.len() {
            match self.get_expr(states[i]) {
                // Flatten nested unions
                AExpr::Union(nested) => {
                    // Remove the current state that's a union
                    states.remove(i);
                    
                    // Add all nested states
                    for &nested_state in nested {
                        states.push(nested_state);
                    }
                },
                // Collect SPPs for combining
                AExpr::SPP(spp) => {
                    combined_spp = self.spp.union(combined_spp, *spp);
                    states.remove(i);
                },
                // Keep everything else
                _ => i += 1,
            }
        }
        
        // Sort and remove duplicates
        states.sort();
        states.dedup();
        
        // Add the combined SPP back in if it's not zero
        if combined_spp != self.spp.zero {
            let spp_state = self.mk_spp(combined_spp);
            states.push(spp_state);
            states.sort();
        }
        
        states
    }

    fn mk_union(&mut self, e1: State, e2: State) -> State {
        if e1 == e2 {
            return e1;
        }

        // Handle special cases
        match self.get_expr(e1) {
            AExpr::SPP(s) => {
                if *s == self.spp.zero {
                    return e2;
                }
            },
            _ => {}
        }
        
        match self.get_expr(e2) {
            AExpr::SPP(s) => {
                if *s == self.spp.zero {
                    return e1;
                }
            },
            _ => {}
        }

        // Create initial vector with e1 and e2
        let mut states = vec![e1, e2];
        
        // Canonicalize the union
        states = self.canonicalize_union(states);
        
        // Special case: empty vector after canonicalization
        if states.is_empty() {
            return self.mk_spp(self.spp.zero);
        }
        
        // Special case: just one element
        if states.len() == 1 {
            return states[0];
        }
        
        // Create an n-ary union
        self.intern(AExpr::Union(states))
    }

    /// Canonicalizes a list of states for intersection
    /// 1. Flattens nested intersections
    /// 2. Sorts and removes duplicates
    /// 3. Combines all SPP children
    fn canonicalize_intersect(&mut self, mut states: Vec<State>) -> Vec<State> {
        // Keep track of SPPs to be combined
        let mut combined_spp = self.spp.top;
        
        // Process all states to collect nested intersections and SPPs
        let mut i = 0;
        while i < states.len() {
            match self.get_expr(states[i]) {
                // Flatten nested intersections
                AExpr::Intersect(nested) => {
                    // Remove the current state that's an intersection
                    states.remove(i);
                    
                    // Add all nested states
                    for &nested_state in nested {
                        states.push(nested_state);
                    }
                },
                // Collect SPPs for combining
                AExpr::SPP(spp) => {
                    combined_spp = self.spp.intersect(combined_spp, *spp);
                    states.remove(i);
                },
                // Keep everything else
                _ => i += 1,
            }
        }
        
        // Sort and remove duplicates
        states.sort();
        states.dedup();
        
        // Add the combined SPP back in if it's not one
        if combined_spp != self.spp.top {
            let spp_state = self.mk_spp(combined_spp);
            states.push(spp_state);
            states.sort();
        }
        
        states
    }

    fn mk_intersect(&mut self, e1: State, e2: State) -> State {
        if e1 == e2 {
            return e1;
        }

        // Handle special cases
        match self.get_expr(e1) {
            AExpr::SPP(s) => {
                if *s == self.spp.one {
                    return e2;
                }
                if *s == self.spp.zero {
                    return self.mk_spp(self.spp.zero);
                }
            },
            _ => {}
        }
        
        match self.get_expr(e2) {
            AExpr::SPP(s) => {
                if *s == self.spp.one {
                    return e1;
                }
                if *s == self.spp.zero {
                    return self.mk_spp(self.spp.zero);
                }
            },
            _ => {}
        }

        // SPP Simplification: s1 & s2
        if let (AExpr::SPP(s1), AExpr::SPP(s2)) = (self.get_expr(e1), self.get_expr(e2)) {
            let result_spp = self.spp.intersect(*s1, *s2);
            return self.mk_spp(result_spp);
        }

        // Create initial vector with e1 and e2
        let mut states = vec![e1, e2];
        
        // Canonicalize the intersection
        states = self.canonicalize_intersect(states);
        
        // Special case: empty vector after canonicalization
        if states.is_empty() {
            return self.mk_spp(self.spp.one);
        }
        
        // Special case: just one element
        if states.len() == 1 {
            return states[0];
        }
        
        // Create an n-ary intersection
        self.intern(AExpr::Intersect(states))
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
        
        // De Morgan's laws and complement simplifications
        let expr = self.get_expr(e).clone();
        match expr {
            AExpr::Union(states) => {
                // !(e1 + e2 + ... + en) = !e1 & !e2 & ... & !en
                if states.is_empty() {
                    return self.mk_spp(self.spp.one); // Complement of empty union is 1
                }
                
                // Avoid borrowing self by calculating complements first
                let complements: Vec<State> = states.iter()
                    .map(|&state| self.mk_complement(state))
                    .collect();
                
                // Then combine them with intersect
                if complements.is_empty() {
                    return self.mk_spp(self.spp.one);
                }
                
                if complements.len() == 1 {
                    return complements[0];
                }
                
                return self.intern(AExpr::Intersect(complements));
            }
            AExpr::Intersect(states) => {
                // !(e1 & e2 & ... & en) = !e1 + !e2 + ... + !en
                if states.is_empty() {
                    return self.mk_spp(self.spp.zero); // Complement of empty intersection is 0
                }
                
                // Avoid borrowing self by calculating complements first
                let complements: Vec<State> = states.iter()
                    .map(|&state| self.mk_complement(state))
                    .collect();
                
                if complements.is_empty() {
                    return self.mk_spp(self.spp.zero);
                }
                
                if complements.len() == 1 {
                    return complements[0];
                }
                
                return self.intern(AExpr::Union(complements));
            }
            AExpr::Difference(e1, e2) => {
                let c1 = self.mk_complement(e1);
                return self.mk_union(c1, e2);
            }
            AExpr::Xor(e1, e2) => {
                let c1 = self.mk_complement(e1);
                return self.mk_xor(c1, e2);
            }
            _ => {}
        }
        
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

        // TODO: Add check for SPP zero (0 ; e = 0, e ; 0 = 0)
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

        self.intern(AExpr::Star(e))
    }

    fn mk_dup(&mut self) -> State {
        self.intern(AExpr::Dup)
    }

    fn mk_top(&mut self) -> State {
        self.intern(AExpr::Top)
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
        }
    }

    // --- Symbolic transitions: ST ---

    pub fn st_empty(&mut self) -> ST {
        ST::new(HashMap::new())
    }

    pub fn st_singleton(&mut self, spp: spp::SPP, state: State) -> ST {
        ST::new(HashMap::from([(state, spp)]))
    }

    /// Insert a transition into a ST.
    /// Precondition: spp is disjoint from all other spp's in the ST
    pub fn st_insert_unsafe(&mut self, st: &mut ST, state: State, spp: spp::SPP) {
        // Assert that spp is disjoint from all other spp's in the ST
        #[cfg(debug_assertions)]
        for (_, existing_spp) in st.transitions.iter() {
            debug_assert!(
                self.spp.intersect(*existing_spp, spp) == self.spp.zero,
                "spp must be disjoint from all other spp's in the ST"
            );
        }
        // Check if the state already exists, if so union the spp's
        if let Some(existing_spp) = st.transitions.get_mut(&state) {
            *existing_spp = self.spp.union(*existing_spp, spp);
        } else {
            // Check if the spp is 0, if so don't insert
            if spp == self.spp.zero {
                return;
            }
            // Check if the State is zero, if so don't insert
            if state == self.mk_spp(self.spp.zero) {
                return;
            }
            st.transitions.insert(state, spp);
        }
    }

    /// Insert a transition into a ST.
    pub fn st_insert(&mut self, st: &mut ST, state: State, spp: spp::SPP) {
        // We have to be careful here because a naive implementation would not result in a deterministic ST
        // Strategy: intersect the spp with all other spp's in the ST, and insert an expr union for those
        // Separately keep track of the remaining spp that is inserted separately
        // Check if the state is zero, if so don't insert
        if state == self.mk_spp(self.spp.zero) {
            return;
        }
        let transitions = st.transitions.clone();
        let mut remaining_spp = spp;
        for (state2, spp2) in transitions {
            if remaining_spp == self.spp.zero {
                return;
            }
            let intersect_spp = self.spp.intersect(remaining_spp, spp2);
            let union_state = self.mk_union(state, state2);
            st.transitions.insert(union_state, intersect_spp);
            remaining_spp = self.spp.difference(remaining_spp, intersect_spp);
        }

        st.transitions.insert(state, remaining_spp);
    }

    fn st_intersect(&mut self, st1: ST, st2: ST) -> ST {
        let mut result = ST::empty();
        for (state1, spp1) in &st1.transitions {
            for (state2, spp2) in &st2.transitions {
                let intersect_state = self.mk_intersect(*state1, *state2);
                let spp = self.spp.intersect(*spp1, *spp2);
                self.st_insert_unsafe(&mut result, intersect_state, spp);
            }
        }
        result
    }

    pub fn st_union(&mut self, st1: ST, st2: ST) -> ST {
        let st1_complement = self.st_complement(st1);
        let st2_complement = self.st_complement(st2);
        let st = self.st_intersect(st1_complement, st2_complement);
        self.st_complement(st)
    }

    fn st_difference(&mut self, st1: ST, st2: ST) -> ST {
        // (st1 - st2) = st1 & !st2
        let st2_complement = self.st_complement(st2);
        self.st_intersect(st1, st2_complement)
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
            self.st_insert_unsafe(&mut result, new_state, spp);
        }
        // Find the union of all the spp's in the transitions
        let mut union_spp = self.spp.zero;
        for (_, &spp) in &st.transitions {
            union_spp = self.spp.union(union_spp, spp);
        }
        // Add a transition from the complement of the union to the Top state
        let complement_spp = self.spp.complement(union_spp);
        let top = self.mk_top();
        self.st_insert_unsafe(&mut result, top, complement_spp);
        result
    }

    fn st_postcompose(&mut self, st: ST, expr: State) -> ST {
        let mut result = ST::empty();
        for (state, spp) in st.transitions {
            let new_state = self.mk_sequence(state, expr);
            self.st_insert_unsafe(&mut result, new_state, spp);
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

    // --- Automaton construction: delta, epsilon ---

    pub fn delta(&mut self, state: State) -> ST {
        self.num_calls += 1;
        if self.num_calls > 50 {
            println!("Delta called {} times, artificial limit reached", self.num_calls);
            return ST::empty();
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
                let delta_values: Vec<ST> = states.iter()
                    .map(|&s| self.delta(s))
                    .collect();
                
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
            AExpr::LtlNext(_) => todo!("Implement delta for LTL Next"),
            AExpr::LtlUntil(_, _) => todo!("Implement delta for LTL Until"),
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
                let epsilon_values: Vec<spp::SPP> = states.iter()
                    .map(|&s| self.epsilon(s))
                    .collect();
                
                // Then combine them
                let mut result = self.spp.zero;
                for &eps in &epsilon_values {
                    result = self.spp.union(result, eps);
                }
                result
            }
            AExpr::Intersect(states) => {
                // Pre-compute all epsilon values
                let epsilon_values: Vec<spp::SPP> = states.iter()
                    .map(|&s| self.epsilon(s))
                    .collect();
                
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
            AExpr::LtlNext(_) => todo!("Implement epsilon for LTL Next"),
            AExpr::LtlUntil(_, _) => todo!("Implement epsilon for LTL Until"),
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
    
    /// Returns a string representation of the AExpr for the given state
    pub fn state_to_string(&self, state: State) -> String {
        match self.get_expr(state) {
            AExpr::SPP(spp) => format!("SPP({})", spp),
            AExpr::Union(states) => {
                let state_strings: Vec<String> = states.iter().map(|&s| self.state_to_string(s)).collect();
                format!("({})", state_strings.join(" + "))
            }
            AExpr::Intersect(states) => {
                let state_strings: Vec<String> = states.iter().map(|&s| self.state_to_string(s)).collect();
                format!("({})", state_strings.join(" & "))
            }
            AExpr::Xor(e1, e2) => format!("({} ^ {})", self.state_to_string(*e1), self.state_to_string(*e2)),
            AExpr::Difference(e1, e2) => format!("({} - {})", self.state_to_string(*e1), self.state_to_string(*e2)),
            AExpr::Complement(e) => format!("!{}", self.state_to_string(*e)),
            AExpr::Sequence(e1, e2) => format!("({} ; {})", self.state_to_string(*e1), self.state_to_string(*e2)),
            AExpr::Star(e) => format!("({})*", self.state_to_string(*e)),
            AExpr::Dup => "dup".to_string(),
            AExpr::LtlNext(e) => format!("X({})", self.state_to_string(*e)),
            AExpr::LtlUntil(e1, e2) => format!("({} U {})", self.state_to_string(*e1), self.state_to_string(*e2)),
            AExpr::Top => "⊤".to_string(),
        }
    }
    
    /// Collects all SPP indices from the expression at the given state
    pub fn collect_spps(&self, state: State, spps: &mut HashSet<spp::SPP>) {
        match self.get_expr(state) {
            AExpr::SPP(spp) => {
                spps.insert(*spp);
            },
            AExpr::Union(states) => {
                for &s in states {
                    self.collect_spps(s, spps);
                }
            },
            AExpr::Intersect(states) => {
                for &s in states {
                    self.collect_spps(s, spps);
                }
            },
            AExpr::Xor(e1, e2) | AExpr::Difference(e1, e2) | AExpr::Sequence(e1, e2) | AExpr::LtlUntil(e1, e2) => {
                self.collect_spps(*e1, spps);
                self.collect_spps(*e2, spps);
            },
            AExpr::Complement(e) | AExpr::Star(e) | AExpr::LtlNext(e) => {
                self.collect_spps(*e, spps);
            },
            AExpr::Dup | AExpr::Top => {},
        }
    }
}
