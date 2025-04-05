use crate::spp;
use crate::sp;
use crate::st::ST;

// An AExpr represents an automaton state.
// This is essentially a compressed and hash-consed form of a NetKAT expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum AExpr {
    SPP(spp::SPP), // We keep field tests and mutations and combinations thereof in SPP form
    Union(AExp, AExp),      // e1 + e2
    Intersect(AExp, AExp),  // e1 & e2
    Xor(AExp, AExp),        // e1 ^ e2
    Difference(AExp, AExp), // e1 - e2
    Complement(AExp),      // !e1
    Sequence(AExp, AExp),   // e1; e2
    Star(AExp),            // e*
    Dup,                  // dup
    LtlNext(AExp),         // X e
    LtlUntil(AExp, AExp),   // e1 U e2
    End,                    // represents the singleton set containing the empty string
}

type AExp = Box<AExpr>;
 
struct Aut {
    states: Vec<AExp>,
    spp: spp::SPPstore,
    sp: sp::SPstore,
}

impl ST<AExp> {
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

    fn postcompose(st: Self, expr: AExp) -> Self {
        todo!()
    }
}

impl Aut {
    pub fn new(num_vars: u32) -> Self {
        Aut {
            states: vec![],
            spp: spp::SPPstore::new(num_vars),
            sp: sp::SPstore::new(num_vars),
        }
    }

    pub fn delta(&mut self, expr: AExp) -> ST<AExp> {
        match *expr {
            AExpr::SPP(spp) => ST::singleton(spp, Box::new(AExpr::End)),
            AExpr::Union(e1, e2) => ST::union(self.delta(e1), self.delta(e2)),
            AExpr::Intersect(e1, e2) => ST::intersect(self.delta(e1), self.delta(e2)),
            AExpr::Xor(e1, e2) => ST::xor(self.delta(e1), self.delta(e2)),
            AExpr::Difference(e1, e2) => ST::difference(self.delta(e1), self.delta(e2)),
            AExpr::Complement(e) => ST::complement(self.delta(e)),
            AExpr::Sequence(e1, e2) => {
                // delta(e1 e2) = delta(e1) e2 + epsilon(e1) delta(e2)
                let delta_e1_e2 = ST::postcompose(self.delta(e1.clone()), e2.clone());
                if self.epsilon(e1) {
                    ST::union(delta_e1_e2, self.delta(e2))
                } else {
                    delta_e1_e2
                }
            }
            AExpr::Star(e) => {
                // delta(e*) = epsilon(e*) delta(e) e*, epsilon(e*) = true
                ST::postcompose(self.delta(e.clone()), Box::new(AExpr::Star(e)))
            }
            AExpr::Dup => ST::singleton(self.spp.one, Box::new(AExpr::SPP(self.spp.one))),
            AExpr::LtlNext(_) => todo!(),
            AExpr::LtlUntil(_, _) => todo!(),
            AExpr::End => ST::empty(),
        }
    }

    pub fn epsilon(&self, state: AExp) -> bool {
        match *state {
            AExpr::SPP(_) => false,
            AExpr::Union(e1, e2) => self.epsilon(e1) || self.epsilon(e2),
            AExpr::Intersect(e1, e2) => self.epsilon(e1) && self.epsilon(e2),
            AExpr::Xor(e1, e2) => self.epsilon(e1) ^ self.epsilon(e2),
            AExpr::Difference(e1, e2) => self.epsilon(e1) && !self.epsilon(e2),
            AExpr::Complement(e) => !self.epsilon(e),
            AExpr::Sequence(e1, e2) => self.epsilon(e1) && self.epsilon(e2),
            AExpr::Star(_) => true,
            AExpr::Dup => false,
            AExpr::LtlNext(_) => todo!(),
            AExpr::LtlUntil(_, _) => todo!(),
            AExpr::End => true,
        }
    }
}