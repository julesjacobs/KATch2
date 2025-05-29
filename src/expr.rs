use crate::pre::{Field, Value};

/// Represents NetKAT expressions with LTL extensions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Zero,                 // 0
    One,                  // 1
    Top,                  // T
    Assign(Field, Value), // field := value
    Test(Field, Value),   // field == value
    BitRangeAssign(Field, Field, Vec<bool>), // x[start..end] := value (as bits)
    BitRangeTest(Field, Field, Vec<bool>),   // x[start..end] == value (as bits)
    Union(Exp, Exp),      // e1 + e2
    Intersect(Exp, Exp),  // e1 & e2
    Xor(Exp, Exp),        // e1 ^ e2
    Difference(Exp, Exp), // e1 - e2
    Complement(Exp),      // ~e1
    TestNegation(Exp),    // !e (test fragment only)
    IfThenElse(Exp, Exp, Exp), // if e1 then e2 else e3 (e1 must be test fragment)
    Var(String),          // Variable reference
    Let(String, Exp, Exp), // let x = e1 in e2
    Sequence(Exp, Exp),   // e1; e2
    Star(Exp),            // e*
    Dup,                  // dup
    LtlNext(Exp),         // X e
    LtlUntil(Exp, Exp),   // e1 U e2
    End,                  // end
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
    pub fn bit_range_assign(start: Field, end: Field, bits: Vec<bool>) -> Exp {
        Box::new(Expr::BitRangeAssign(start, end, bits))
    }
    pub fn bit_range_test(start: Field, end: Field, bits: Vec<bool>) -> Exp {
        Box::new(Expr::BitRangeTest(start, end, bits))
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
    pub fn test_negation(e: Exp) -> Exp {
        Box::new(Expr::TestNegation(e))
    }
    pub fn if_then_else(cond: Exp, then_expr: Exp, else_expr: Exp) -> Exp {
        Box::new(Expr::IfThenElse(cond, then_expr, else_expr))
    }
    pub fn var(name: String) -> Exp {
        Box::new(Expr::Var(name))
    }
    pub fn let_in(var_name: String, def: Exp, body: Exp) -> Exp {
        Box::new(Expr::Let(var_name, def, body))
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
    /// Weak next operator `(X' e = X e \/ end)`
    pub fn ltl_weak_next(e: Exp) -> Exp {
        Box::new(Expr::Union(Expr::ltl_next(e), Expr::end()))
    }
    pub fn ltl_until(e1: Exp, e2: Exp) -> Exp {
        Box::new(Expr::LtlUntil(e1, e2))
    }
    /// Weak until operator `(e1 W e2 = e1 U e2 \/ G e1)`
    pub fn ltl_weak_until(e1: Exp, e2: Exp) -> Exp {
        Box::new(Expr::Union(
            Expr::ltl_until(e1.clone(), e2),
            Expr::ltl_globally(e1),
        ))
    }
    pub fn end() -> Exp {
        Box::new(Expr::End)
    }

    /// Check if an expression is in the test fragment
    /// Test fragment consists of: 0, 1, logical operators (+, &, ^, -), 
    /// field tests (==), test negation (!), and sequence (;)
    pub fn is_test_fragment(&self) -> bool {
        match self {
            Expr::Zero | Expr::One => true,
            Expr::Test(_, _) | Expr::BitRangeTest(_, _, _) => true,
            Expr::Union(e1, e2) | Expr::Intersect(e1, e2) | 
            Expr::Xor(e1, e2) | Expr::Difference(e1, e2) |
            Expr::Sequence(e1, e2) => e1.is_test_fragment() && e2.is_test_fragment(),
            Expr::TestNegation(e) => e.is_test_fragment(),
            // Everything else is not in test fragment
            Expr::Top | Expr::Assign(_, _) | Expr::BitRangeAssign(_, _, _) | 
            Expr::Complement(_) | Expr::Star(_) | Expr::Dup | Expr::LtlNext(_) | 
            Expr::LtlUntil(_, _) | Expr::End | Expr::IfThenElse(_, _, _) |
            Expr::Var(_) | Expr::Let(_, _, _) => false,
        }
    }

    pub fn num_fields(&self) -> u32 {
        match self {
            Expr::Zero | Expr::One | Expr::Top | Expr::Dup | Expr::End => 0,
            Expr::Assign(field, _) | Expr::Test(field, _) => field + 1,
            Expr::BitRangeAssign(_start, end, _) | Expr::BitRangeTest(_start, end, _) => end + 1,
            Expr::Union(e1, e2)
            | Expr::Intersect(e1, e2)
            | Expr::Xor(e1, e2)
            | Expr::Difference(e1, e2)
            | Expr::Sequence(e1, e2)
            | Expr::LtlUntil(e1, e2) => e1.num_fields().max(e2.num_fields()),
            Expr::IfThenElse(cond, then_expr, else_expr) => {
                cond.num_fields().max(then_expr.num_fields()).max(else_expr.num_fields())
            }
            Expr::Let(_, def, body) => def.num_fields().max(body.num_fields()),
            Expr::Complement(e) | Expr::TestNegation(e) | Expr::Star(e) | Expr::LtlNext(e) => e.num_fields(),
            Expr::Var(_) => 0, // Variables don't directly reference fields
        }
    }

    /// Helper function for constructing `F e` using the equivalence `F e ≡ true U e`
    pub fn ltl_finally(e: Exp) -> Exp {
        Box::new(Expr::LtlUntil(Expr::top(), e))
    }

    // Helper function for constructing `G e` using the equivalence `G e ≡ ¬(true U ¬e)`
    pub fn ltl_globally(e: Exp) -> Exp {
        let until_expr = Expr::ltl_until(Expr::top(), Expr::complement(e));
        Box::new(Expr::Complement(until_expr))
    }

    // Helper function for constructing `e1 R e2` (weak release) using the equivalence `e1 R e2 ≡ ¬(¬e1 U ¬e2)`
    pub fn ltl_release(e1: Exp, e2: Exp) -> Exp {
        let not_e1 = Expr::complement(e1);
        let not_e2 = Expr::complement(e2);
        let until_expr = Expr::ltl_until(not_e1, not_e2);
        Box::new(Expr::Complement(until_expr))
    }

    /// Strong release operator `(e1 M e2 = e1 R e2 /\ F e1)`
    pub fn ltl_strong_release(e1: Exp, e2: Exp) -> Exp {
        Box::new(Expr::Intersect(
            Expr::ltl_release(e1.clone(), e2),
            Expr::ltl_finally(e1),
        ))
    }
    
    /// Substitute all occurrences of variable `var` with expression `replacement` in `self`
    pub fn substitute(&self, var: &str, replacement: &Expr) -> Exp {
        match self {
            // Base cases
            Expr::Zero => Expr::zero(),
            Expr::One => Expr::one(),
            Expr::Top => Expr::top(),
            Expr::Dup => Expr::dup(),
            Expr::End => Expr::end(),
            Expr::Assign(f, v) => Expr::assign(*f, *v),
            Expr::Test(f, v) => Expr::test(*f, *v),
            
            // Variable case - this is where substitution happens
            Expr::Var(name) => {
                if name == var {
                    Box::new(replacement.clone())
                } else {
                    Expr::var(name.clone())
                }
            }
            
            // Recursive cases
            Expr::Union(e1, e2) => Expr::union(e1.substitute(var, replacement), e2.substitute(var, replacement)),
            Expr::Intersect(e1, e2) => Expr::intersect(e1.substitute(var, replacement), e2.substitute(var, replacement)),
            Expr::Xor(e1, e2) => Expr::xor(e1.substitute(var, replacement), e2.substitute(var, replacement)),
            Expr::Difference(e1, e2) => Expr::difference(e1.substitute(var, replacement), e2.substitute(var, replacement)),
            Expr::Sequence(e1, e2) => Expr::sequence(e1.substitute(var, replacement), e2.substitute(var, replacement)),
            Expr::Complement(e) => Expr::complement(e.substitute(var, replacement)),
            Expr::TestNegation(e) => Expr::test_negation(e.substitute(var, replacement)),
            Expr::Star(e) => Expr::star(e.substitute(var, replacement)),
            Expr::LtlNext(e) => Expr::ltl_next(e.substitute(var, replacement)),
            Expr::LtlUntil(e1, e2) => Expr::ltl_until(e1.substitute(var, replacement), e2.substitute(var, replacement)),
            Expr::IfThenElse(c, t, e) => Expr::if_then_else(
                c.substitute(var, replacement),
                t.substitute(var, replacement),
                e.substitute(var, replacement)
            ),
            
            // Let binding case - careful with variable shadowing
            Expr::Let(bound_var, def, body) => {
                if bound_var == var {
                    // Variable is shadowed, don't substitute in body
                    Expr::let_in(bound_var.clone(), def.substitute(var, replacement), body.clone())
                } else {
                    // Substitute in both definition and body
                    Expr::let_in(bound_var.clone(), def.substitute(var, replacement), body.substitute(var, replacement))
                }
            }
            
            // Bit range operations don't contain variables
            Expr::BitRangeAssign(start, end, value) => Expr::bit_range_assign(*start, *end, value.clone()),
            Expr::BitRangeTest(start, end, value) => Expr::bit_range_test(*start, *end, value.clone()),
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Zero => write!(f, "0"),
            Expr::One => write!(f, "1"),
            Expr::Top => write!(f, "⊤"),
            Expr::Assign(field, value) => {
                write!(f, "x{} := {}", field, if *value { "1" } else { "0" })
            }
            Expr::Test(field, value) => {
                write!(f, "x{} == {}", field, if *value { "1" } else { "0" })
            }
            Expr::Union(e1, e2) => write!(f, "({} + {})", e1, e2),
            Expr::Intersect(e1, e2) => write!(f, "({} & {})", e1, e2),
            Expr::Xor(e1, e2) => write!(f, "({} ^ {})", e1, e2),
            Expr::Difference(e1, e2) => write!(f, "({} - {})", e1, e2),
            Expr::Complement(e) => write!(f, "~{}", e),
            Expr::TestNegation(e) => write!(f, "!{}", e),
            Expr::IfThenElse(cond, then_expr, else_expr) => {
                write!(f, "(if {} then {} else {})", cond, then_expr, else_expr)
            }
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Let(var, def, body) => write!(f, "(let {} = {} in {})", var, def, body),
            Expr::Sequence(e1, e2) => write!(f, "({} ; {})", e1, e2),
            Expr::Star(e) => write!(f, "({})*", e),
            Expr::Dup => write!(f, "dup"),
            Expr::LtlNext(e) => write!(f, "X({})", e),
            Expr::LtlUntil(e1, e2) => write!(f, "({} U {})", e1, e2),
            Expr::End => write!(f, "end"),
            Expr::BitRangeAssign(start, end, value) => {
                let num = bits_to_number(value);
                write!(f, "x[{}..{}] := {}", start, end, num)
            }
            Expr::BitRangeTest(start, end, value) => {
                let num = bits_to_number(value);
                write!(f, "x[{}..{}] == {}", start, end, num)
            }
        }
    }
}

// Helper function to convert bit vector to number for display
fn bits_to_number(bits: &[bool]) -> u128 {
    bits.iter().enumerate().fold(0u128, |acc, (i, &bit)| {
        if bit { acc | (1 << i) } else { acc }
    })
}
