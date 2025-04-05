use crate::expr::Expr;
use crate::spp;
use crate::sp;
use crate::st::ST;
use std::collections::HashMap;

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
}

// AExp is an index into the Aut's expression table.
type State = usize;
 
struct Aut {
    aexprs: Vec<AExpr>,
    aexpr_map: HashMap<AExpr, State>,
    spp: spp::SPPstore,
    sp: sp::SPstore,
}

impl ST<State> {
    fn union(st1: Self, st2: Self) -> Self {
        todo!()
    }

    fn intersect(st1: Self, st2: Self) -> Self {
        todo!()
    }

    fn xor(st1: Self, st2: Self) -> Self {
        todo!()
    }

    fn difference(st1: Self, st2: Self) -> Self {
        todo!()
    }

    fn complement(st: Self) -> Self {
        todo!()
    }

    fn postcompose(st: Self, expr: State) -> Self {
        todo!()
    }
}

impl Aut {
    pub fn new(num_vars: u32) -> Self {
        let mut aut = Aut {
            aexprs: vec![],
            aexpr_map: HashMap::new(),
            spp: spp::SPPstore::new(num_vars),
            sp: sp::SPstore::new(num_vars),
        };
        aut
    }

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
        
        // SPP Simplification: !s
        if let AExpr::SPP(s) = self.get_expr(e) {
            let result_spp = self.spp.complement(s); 
            return self.mk_spp(result_spp);
        }
        
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

    pub fn delta(&mut self, expr_id: State) -> ST<State> {
        match self.get_expr(expr_id) {
            AExpr::SPP(spp) => ST::singleton(spp, self.mk_end()),
            AExpr::Union(e1, e2) => ST::union(self.delta(e1), self.delta(e2)),
            AExpr::Intersect(e1, e2) => ST::intersect(self.delta(e1), self.delta(e2)),
            AExpr::Xor(e1, e2) => ST::xor(self.delta(e1), self.delta(e2)),
            AExpr::Difference(e1, e2) => ST::difference(self.delta(e1), self.delta(e2)),
            AExpr::Complement(e) => ST::complement(self.delta(e)),
            AExpr::Sequence(e1, e2) => {
                // delta(e1 ; e2) = delta(e1) ; e2 + epsilon(e1) delta(e2)
                let delta_e1_seq_e2 = ST::postcompose(self.delta(e1), e2); // Pass index e2
                if self.epsilon(e1) {
                    ST::union(delta_e1_seq_e2, self.delta(e2))
                } else {
                    delta_e1_seq_e2
                }
            }
            AExpr::Star(e) => {
                // delta(e*) = delta(e) ; e*
                // Note: epsilon(e*) is always true, handled implicitly by the definition of Star.
                // We need to create the AExp for e* first.
                let star_e = self.mk_star(e); // Get the index for e*
                ST::postcompose(self.delta(e), star_e) // Pass index star_e
            }
            AExpr::Dup => {
                // delta(dup) = 1 ; SPP(1)  (Assuming SPP(1) means the identity mutation)
                // Need to create SPP(1) via smart constructor
                let spp_one = self.mk_spp(self.spp.one);
                ST::singleton(self.spp.one, spp_one)
            }
            AExpr::LtlNext(_) => todo!(),
            AExpr::LtlUntil(_, _) => todo!(),
            AExpr::End => ST::empty(),
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
            AExpr::LtlNext(_) => todo!(), // Epsilon of LTL Next should likely be false
            AExpr::LtlUntil(_, _) => todo!(), // Epsilon of LTL Until depends on the first argument
            AExpr::End => true,
        }
    }
}