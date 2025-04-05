use crate::expr::Expr;
use crate::spp;
use crate::sp;
use std::collections::HashMap;
use std::hash::Hash;
// An AExpr represents an automaton state.
// This is essentially a compressed and hash-consed form of a NetKAT expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
enum AExpr {
    SPP(spp::SPP), // We keep field tests and mutations and combinations thereof in SPP form
    Union(State, State),      // e1 + e2
    Intersect(State, State),  // e1 & e2
    Xor(State, State),        // e1 ^ e2
    Difference(State, State), // e1 - e2
    Complement(State),      // !e1
    Sequence(State, State),   // e1; e2
    Star(State),            // e*
    Dup,                  // dup
    LtlNext(State),         // X e
    LtlUntil(State, State),   // e1 U e2
    End,                    // represents the singleton set containing the empty string
    Top,                    // represents the set of all strings
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
        ST { transitions: HashMap::new() }
    }
}
 
struct Aut {
    aexprs: Vec<AExpr>,
    aexpr_map: HashMap<AExpr, State>,
    spp: spp::SPPstore,
    sp: sp::SPstore,
}

impl Aut {
    pub fn new(num_vars: u32) -> Self {
        let aut = Aut {
            aexprs: vec![],
            aexpr_map: HashMap::new(),
            spp: spp::SPPstore::new(num_vars),
            sp: sp::SPstore::new(num_vars),
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
        self.aexprs.push(expr);
        self.aexpr_map.insert(expr, id);
        id
    }

    // Smart Constructors with Simplifications

    fn mk_spp(&mut self, spp: spp::SPP) -> State {
        // TODO: Add simplification for SPP constants (0, 1) if not handled by SPPstore itself
        self.intern(AExpr::SPP(spp))
    }

    fn mk_union(&mut self, e1: State, e2: State) -> State {
        if e1 == e2 { return e1; }
        
        // SPP Simplification: s1 + s2
        if let (AExpr::SPP(s1), AExpr::SPP(s2)) = (self.get_expr(e1), self.get_expr(e2)) {
            let result_spp = self.spp.union(s1, s2); 
            return self.mk_spp(result_spp);
        }
        
        // TODO: Add check for SPP zero/one (e + 0 = e, e + 1 = 1)
        // Canonical ordering: Ensure smaller index comes first for commutative ops
        let (e1, e2) = if e1 < e2 { (e1, e2) } else { (e2, e1) };
        self.intern(AExpr::Union(e1, e2))
    }

    fn mk_intersect(&mut self, e1: State, e2: State) -> State {
        if e1 == e2 { return e1; }

        // SPP Simplification: s1 & s2
        if let (AExpr::SPP(s1), AExpr::SPP(s2)) = (self.get_expr(e1), self.get_expr(e2)) {
            let result_spp = self.spp.intersect(s1, s2); 
            return self.mk_spp(result_spp);
        }

        // TODO: Add check for SPP zero/one (e & 0 = 0, e & 1 = e)
        // Canonical ordering
        let (e1, e2) = if e1 < e2 { (e1, e2) } else { (e2, e1) };
        self.intern(AExpr::Intersect(e1, e2))
    }

    fn mk_xor(&mut self, e1: State, e2: State) -> State {
        if e1 == e2 { return self.mk_spp(self.spp.zero); } // e ^ e = 0

        // SPP Simplification: s1 ^ s2
        if let (AExpr::SPP(s1), AExpr::SPP(s2)) = (self.get_expr(e1), self.get_expr(e2)) {
            let result_spp = self.spp.xor(s1, s2); 
            return self.mk_spp(result_spp);
        }

        // Canonical ordering
        let (e1, e2) = if e1 < e2 { (e1, e2) } else { (e2, e1) };
        self.intern(AExpr::Xor(e1, e2))
    }

    fn mk_difference(&mut self, e1: State, e2: State) -> State {
        if e1 == e2 { return self.mk_spp(self.spp.zero); } // e - e = 0

        // SPP Simplification: s1 - s2
        if let (AExpr::SPP(s1), AExpr::SPP(s2)) = (self.get_expr(e1), self.get_expr(e2)) {
            let result_spp = self.spp.difference(s1, s2);
            return self.mk_spp(result_spp);
        }

        // TODO: Add check for SPP zero (e - 0 = e, 0 - e = 0? depends if SPP is bool or set)
        self.intern(AExpr::Difference(e1, e2))
    }

    fn mk_complement(&mut self, e: State) -> State {
        // Simplify !!e = e
        if let AExpr::Complement(inner_e) = self.get_expr(e) {
            return inner_e;
        }
        
        // SPP Simplification is not valid here, since we are complementing a set of strings!
        
        self.intern(AExpr::Complement(e))
    }

    fn mk_sequence(&mut self, e1: State, e2: State) -> State {
        // Simplify e ; End = e
        let end_id = self.intern(AExpr::End); // Ensure End is interned if not already
        if e2 == end_id { return e1; }
        // Simplify End ; e = e
        if e1 == end_id { return e2; }
        
        // SPP Simplification: s1 ; s2
        if let (AExpr::SPP(s1), AExpr::SPP(s2)) = (self.get_expr(e1), self.get_expr(e2)) {
            let result_spp = self.spp.sequence(s1, s2);
            return self.mk_spp(result_spp);
        }

        // TODO: Add check for SPP zero (0 ; e = 0, e ; 0 = 0)
        self.intern(AExpr::Sequence(e1, e2))
    }

    fn mk_star(&mut self, e: State) -> State {
        // Simplify End* = End
        let end_id = self.intern(AExpr::End);
        if e == end_id { return end_id; }
        // Simplify (e*)* = e*
        if let AExpr::Star(_) = self.get_expr(e) { // Check the type without getting inner_e
            return e; // Return the existing e* index
        }
        self.intern(AExpr::Star(e))
    }

    fn mk_dup(&mut self) -> State {
        self.intern(AExpr::Dup)
    }

    fn mk_top(&mut self) -> State {
        self.intern(AExpr::Top)
    }

    fn mk_end(&mut self) -> State {
        self.intern(AExpr::End)
    }

    // Helper to get the actual expression from an index
    fn get_expr(&self, id: State) -> AExpr {
        self.aexprs[id]
    }

    // Function to convert an external Expr to an internal AExp index
    pub fn expr_to_state(&mut self, expr: &Expr) -> State {
        match expr {
            Expr::Zero => self.mk_spp(self.spp.zero),
            Expr::One => self.mk_end(), // Map Expr::One to AExpr::End
            Expr::Top => self.mk_spp(self.spp.top), // Map Expr::Top to SPP Top
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

    pub fn st_insert(&mut self, st: &mut ST, state: State, spp: spp::SPP) {
        // Check if the state already exists, if so union the spp's
        if let Some(existing_spp) = st.transitions.get_mut(&state) {
            *existing_spp = self.spp.union(*existing_spp, spp);
        } else {
            // Check if the spp is 0, if so don't insert
            if spp != self.spp.zero {
                st.transitions.insert(state, spp);
            }
        }
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

    // --- Automaton construction: delta, epsilon ---

    pub fn delta(&mut self, expr_id: State) -> ST {
        match self.get_expr(expr_id) {
            AExpr::SPP(spp) => {
                let end = self.mk_end();
                self.st_singleton(spp, end)
            },
            AExpr::Union(e1, e2) => {
                let delta1 = self.delta(e1);
                let delta2 = self.delta(e2);
                self.st_union(delta1, delta2)
            },
            AExpr::Intersect(e1, e2) => {
                let delta1 = self.delta(e1);
                let delta2 = self.delta(e2);
                self.st_intersect(delta1, delta2)
            },
            AExpr::Xor(e1, e2) => {
                let delta1 = self.delta(e1);
                let delta2 = self.delta(e2);
                self.st_xor(delta1, delta2)
            },
            AExpr::Difference(e1, e2) => {
                let delta1 = self.delta(e1);
                let delta2 = self.delta(e2);
                self.st_difference(delta1, delta2)
            },
            AExpr::Complement(e) => {
                let delta_e = self.delta(e);
                self.st_complement(delta_e)
            },
            AExpr::Sequence(e1, e2) => {
                // delta(e1 ; e2) = delta(e1) ; e2 + epsilon(e1) delta(e2)
                let delta_e1 = self.delta(e1);
                let delta_e1_seq_e2 = self.st_postcompose(delta_e1, e2);
                
                if self.epsilon(e1) {
                    let delta_e2 = self.delta(e2);
                    self.st_union(delta_e1_seq_e2, delta_e2)
                } else {
                    delta_e1_seq_e2
                }
            }
            AExpr::Star(e) => {
                // delta(e*) = delta(e) ; e*
                // Note: epsilon(e*) is always true, handled implicitly by the definition of Star.
                // We need to create the AExp for e* first.
                let star_e = self.mk_star(e); // Get the index for e*
                let delta_e = self.delta(e);
                self.st_postcompose(delta_e, star_e) // Pass index star_e
            }   
            AExpr::Dup => {
                // delta(dup) = 1 ; SPP(1)  (Assuming SPP(1) means the identity mutation)
                // Need to create SPP(1) via smart constructor
                let spp_one = self.mk_spp(self.spp.one);
                self.st_singleton(self.spp.one, spp_one)
            }
            AExpr::LtlNext(_) => todo!("Implement delta for LTL Next"),
            AExpr::LtlUntil(_, _) => todo!("Implement delta for LTL Until"),
            AExpr::End => self.st_empty(),
            AExpr::Top => {
                let top = self.mk_top();
                self.st_singleton(self.spp.top, top)
            }
        }
    }

    pub fn epsilon(&self, state_id: State) -> bool {
        match self.get_expr(state_id) {
            AExpr::SPP(_) => false,
            AExpr::Union(e1, e2) => self.epsilon(e1) || self.epsilon(e2),
            AExpr::Intersect(e1, e2) => self.epsilon(e1) && self.epsilon(e2),
            AExpr::Xor(e1, e2) => self.epsilon(e1) ^ self.epsilon(e2),
            AExpr::Difference(e1, e2) => self.epsilon(e1) && !self.epsilon(e2),
            AExpr::Complement(e) => !self.epsilon(e),
            AExpr::Sequence(e1, e2) => self.epsilon(e1) && self.epsilon(e2),
            AExpr::Star(_) => true,
            AExpr::Dup => false,
            AExpr::LtlNext(_) => todo!("Implement epsilon for LTL Next"),
            AExpr::LtlUntil(_, _) => todo!("Implement epsilon for LTL Until"),
            AExpr::End => true,
            AExpr::Top => true,
        }
    }
}