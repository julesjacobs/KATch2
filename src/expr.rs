use crate::spp;
use crate::pre::{Field, Value};

/// Represents NetKAT expressions with LTL extensions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Zero,                 // 0
    One,                  // 1
    Top,                  // T
    Assign(Field, Value), // field := value
    Test(Field, Value),   // field == value
    Union(Exp, Exp),      // e1 + e2
    Intersect(Exp, Exp),  // e1 & e2
    Xor(Exp, Exp),        // e1 ^ e2
    Difference(Exp, Exp), // e1 - e2
    Complement(Exp),      // !e1
    Sequence(Exp, Exp),   // e1; e2
    Star(Exp),            // e*
    Dup,                  // dup
    LtlNext(Exp),         // X e
    LtlUntil(Exp, Exp),   // e1 U e2
}

/// Represents a boxed expression
pub type Exp = Box<Expr>;

impl Expr {
    pub fn zero() -> Exp {
        Box::new(Expr::Zero)
    }
    pub fn one() -> Exp {
        Box::new(Expr::One)
    }
    pub fn top() -> Exp {
        Box::new(Expr::Top)
    }
    pub fn assign(field: Field, value: Value) -> Exp {
        Box::new(Expr::Assign(field, value))
    }
    pub fn test(field: Field, value: Value) -> Exp {
        Box::new(Expr::Test(field, value))
    }
    pub fn union(e1: Exp, e2: Exp) -> Exp {
        Box::new(Expr::Union(e1, e2))
    }
    pub fn intersect(e1: Exp, e2: Exp) -> Exp {
        Box::new(Expr::Intersect(e1, e2))
    }
    pub fn xor(e1: Exp, e2: Exp) -> Exp {
        Box::new(Expr::Xor(e1, e2))
    }
    pub fn difference(e1: Exp, e2: Exp) -> Exp {
        Box::new(Expr::Difference(e1, e2))
    }
    pub fn complement(e: Exp) -> Exp {
        Box::new(Expr::Complement(e))
    }
    pub fn sequence(e1: Exp, e2: Exp) -> Exp {
        Box::new(Expr::Sequence(e1, e2))
    }
    pub fn star(e: Exp) -> Exp {
        Box::new(Expr::Star(e))
    }
    pub fn dup() -> Exp {
        Box::new(Expr::Dup)
    }
    pub fn ltl_next(e: Exp) -> Exp {
        Box::new(Expr::LtlNext(e))
    }
    pub fn ltl_until(e1: Exp, e2: Exp) -> Exp {
        Box::new(Expr::LtlUntil(e1, e2))
    }
}
