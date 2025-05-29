use crate::expr::{Expr, Exp};

/// Error type for desugaring phase
#[derive(Debug, Clone)]
pub struct DesugarError {
    pub message: String,
}

impl std::fmt::Display for DesugarError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for DesugarError {}

/// Desugar an expression, eliminating TestNegation operators
/// This validates that TestNegation is only applied to test fragments
/// and transforms it using De Morgan's laws
pub fn desugar(expr: &Expr) -> Result<Exp, DesugarError> {
    match expr {
        // Base cases - no transformation needed
        Expr::Zero => Ok(Expr::zero()),
        Expr::One => Ok(Expr::one()),
        Expr::Top => Ok(Expr::top()),
        Expr::Dup => Ok(Expr::dup()),
        Expr::End => Ok(Expr::end()),
        Expr::Assign(f, v) => Ok(Expr::assign(*f, *v)),
        Expr::Test(f, v) => Ok(Expr::test(*f, *v)),
        
        // Recursive cases - desugar subexpressions
        Expr::Union(e1, e2) => {
            let d1 = desugar(e1)?;
            let d2 = desugar(e2)?;
            Ok(Expr::union(d1, d2))
        }
        Expr::Intersect(e1, e2) => {
            let d1 = desugar(e1)?;
            let d2 = desugar(e2)?;
            Ok(Expr::intersect(d1, d2))
        }
        Expr::Xor(e1, e2) => {
            let d1 = desugar(e1)?;
            let d2 = desugar(e2)?;
            Ok(Expr::xor(d1, d2))
        }
        Expr::Difference(e1, e2) => {
            let d1 = desugar(e1)?;
            let d2 = desugar(e2)?;
            Ok(Expr::difference(d1, d2))
        }
        Expr::Sequence(e1, e2) => {
            let d1 = desugar(e1)?;
            let d2 = desugar(e2)?;
            Ok(Expr::sequence(d1, d2))
        }
        Expr::Complement(e) => {
            let d = desugar(e)?;
            Ok(Expr::complement(d))
        }
        Expr::Star(e) => {
            let d = desugar(e)?;
            Ok(Expr::star(d))
        }
        Expr::LtlNext(e) => {
            let d = desugar(e)?;
            Ok(Expr::ltl_next(d))
        }
        Expr::LtlUntil(e1, e2) => {
            let d1 = desugar(e1)?;
            let d2 = desugar(e2)?;
            Ok(Expr::ltl_until(d1, d2))
        }
        
        // TestNegation - the interesting case
        Expr::TestNegation(e) => {
            // First check if e is in the test fragment
            if !e.is_test_fragment() {
                return Err(DesugarError {
                    message: format!("Test negation (!) can only be applied to test fragment expressions, but was applied to: {}", e)
                });
            }
            
            // Now apply De Morgan's laws to eliminate TestNegation
            desugar_test_negation(e)
        }
        
        // IfThenElse - desugar to (cond ; then) + (!cond ; else)
        Expr::IfThenElse(cond, then_expr, else_expr) => {
            // First check if condition is in the test fragment
            if !cond.is_test_fragment() {
                return Err(DesugarError {
                    message: format!("If-then-else condition must be a test fragment expression, but got: {}", cond)
                });
            }
            
            // Desugar all subexpressions
            let d_cond = desugar(cond)?;
            let d_then = desugar(then_expr)?;
            let d_else = desugar(else_expr)?;
            
            // Create negated condition using test negation then desugar it
            let neg_cond = desugar_test_negation(cond)?;
            
            // if c then t else e = (c ; t) + (!c ; e)
            Ok(Expr::union(
                Expr::sequence(d_cond, d_then),
                Expr::sequence(neg_cond, d_else)
            ))
        }
        
        // Let binding - desugar by substitution
        Expr::Let(var, def, body) => {
            // First desugar the definition
            let d_def = desugar(def)?;
            
            // Substitute all occurrences of var with d_def in body
            let substituted_body = body.substitute(var, &d_def);
            
            // Then desugar the substituted body
            desugar(&substituted_body)
        }
        
        // Variable - should have been eliminated by let substitution
        Expr::Var(name) => {
            Err(DesugarError {
                message: format!("Unbound variable '{}' encountered during desugaring", name)
            })
        }
        
        // BitRangeAssign - desugar to sequence of individual assignments
        Expr::BitRangeAssign(start, end, bits) => {
            desugar_bit_range_assign(*start, *end, bits)
        }
        
        // BitRangeTest - desugar to conjunction of individual tests
        Expr::BitRangeTest(start, end, bits) => {
            desugar_bit_range_test(*start, *end, bits)
        }
    }
}

/// Apply De Morgan's laws to eliminate test negation
fn desugar_test_negation(expr: &Expr) -> Result<Exp, DesugarError> {
    match expr {
        // !0 = 1
        Expr::Zero => Ok(Expr::one()),
        
        // !1 = 0
        Expr::One => Ok(Expr::zero()),
        
        // !(x == 0) = x == 1 and !(x == 1) = x == 0
        Expr::Test(f, v) => Ok(Expr::test(*f, !v)),
        
        // !(e1 + e2) = !e1 & !e2 (De Morgan)
        Expr::Union(e1, e2) => {
            let n1 = desugar_test_negation(e1)?;
            let n2 = desugar_test_negation(e2)?;
            Ok(Expr::intersect(n1, n2))
        }
        
        // !(e1 & e2) = !e1 + !e2 (De Morgan)
        Expr::Intersect(e1, e2) => {
            let n1 = desugar_test_negation(e1)?;
            let n2 = desugar_test_negation(e2)?;
            Ok(Expr::union(n1, n2))
        }
        
        // !(e1 ^ e2) = (!e1 & !e2) + (e1 & e2)
        // This is because xor is true when exactly one is true
        // So negation is true when both are false or both are true
        Expr::Xor(e1, e2) => {
            let n1 = desugar_test_negation(e1)?;
            let n2 = desugar_test_negation(e2)?;
            let d1 = desugar(e1)?;
            let d2 = desugar(e2)?;
            Ok(Expr::union(
                Expr::intersect(n1, n2),
                Expr::intersect(d1, d2)
            ))
        }
        
        // !(e1 - e2) = !e1 + e2
        // This is because e1 - e2 means "e1 and not e2"
        // So !(e1 - e2) = !(e1 & !e2) = !e1 + e2
        Expr::Difference(e1, e2) => {
            let n1 = desugar_test_negation(e1)?;
            let d2 = desugar(e2)?;
            Ok(Expr::union(n1, d2))
        }
        
        // !(e1 ; e2) = !e1 + !e2 (for tests, ; is the same as &)
        Expr::Sequence(e1, e2) => {
            let n1 = desugar_test_negation(e1)?;
            let n2 = desugar_test_negation(e2)?;
            Ok(Expr::union(n1, n2))
        }
        
        // !!e = e (double negation)
        Expr::TestNegation(e) => desugar(e),
        
        // BitRangeTest in test fragment
        Expr::BitRangeTest(start, end, bits) => {
            desugar_bit_range_test(*start, *end, bits).and_then(|e| desugar_test_negation(&e))
        }
        
        _ => Err(DesugarError {
            message: format!("Internal error: desugar_test_negation called on non-test fragment expression: {}", expr)
        })
    }
}

/// Desugar bit range assignment to sequence of individual assignments
fn desugar_bit_range_assign(start: u32, end: u32, bits: &[bool]) -> Result<Exp, DesugarError> {
    if bits.len() != (end - start) as usize {
        return Err(DesugarError {
            message: format!("Bit range [{}, {}) expects {} bits, but got {}", 
                           start, end, end - start, bits.len())
        });
    }
    
    if bits.is_empty() {
        // Empty range, return 1 (identity for sequence)
        return Ok(Expr::one());
    }
    
    // Build sequence of assignments (right-associative to match test expectations)
    if bits.len() == 1 {
        return Ok(Expr::assign(start, bits[0]));
    }
    
    // Build from right to left for right-associativity
    let mut result = Expr::assign(start + (bits.len() - 1) as u32, bits[bits.len() - 1]);
    
    for i in (0..bits.len() - 1).rev() {
        let field = start + i as u32;
        result = Expr::sequence(Expr::assign(field, bits[i]), result);
    }
    
    Ok(result)
}

/// Desugar bit range test to conjunction of individual tests
fn desugar_bit_range_test(start: u32, end: u32, bits: &[bool]) -> Result<Exp, DesugarError> {
    if bits.len() != (end - start) as usize {
        return Err(DesugarError {
            message: format!("Bit range [{}, {}) expects {} bits, but got {}", 
                           start, end, end - start, bits.len())
        });
    }
    
    if bits.is_empty() {
        // Empty range, return 1 (identity for intersection)
        return Ok(Expr::one());
    }
    
    // Build conjunction of tests (right-associative to match test expectations)
    if bits.len() == 1 {
        return Ok(Expr::test(start, bits[0]));
    }
    
    // Build from right to left for right-associativity
    let mut result = Expr::test(start + (bits.len() - 1) as u32, bits[bits.len() - 1]);
    
    for i in (0..bits.len() - 1).rev() {
        let field = start + i as u32;
        result = Expr::intersect(Expr::test(field, bits[i]), result);
    }
    
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_desugar_basic() {
        // Test that non-TestNegation expressions are unchanged
        assert_eq!(desugar(&Expr::Zero).unwrap(), Expr::zero());
        assert_eq!(desugar(&Expr::One).unwrap(), Expr::one());
        assert_eq!(desugar(&Expr::Test(0, true)).unwrap(), Expr::test(0, true));
    }
    
    #[test]
    fn test_desugar_test_negation_basic() {
        // !0 = 1
        let expr = Expr::TestNegation(Expr::zero());
        assert_eq!(desugar(&expr).unwrap(), Expr::one());
        
        // !1 = 0
        let expr = Expr::TestNegation(Expr::one());
        assert_eq!(desugar(&expr).unwrap(), Expr::zero());
        
        // !(x0 == 1) = x0 == 0
        let expr = Expr::TestNegation(Expr::test(0, true));
        assert_eq!(desugar(&expr).unwrap(), Expr::test(0, false));
        
        // !(x0 == 0) = x0 == 1
        let expr = Expr::TestNegation(Expr::test(0, false));
        assert_eq!(desugar(&expr).unwrap(), Expr::test(0, true));
    }
    
    #[test]
    fn test_desugar_demorgan() {
        // !(e1 + e2) = !e1 & !e2
        let e1 = Expr::test(0, true);
        let e2 = Expr::test(1, false);
        let expr = Expr::TestNegation(Expr::union(e1, e2));
        let expected = Expr::intersect(
            Expr::test(0, false),
            Expr::test(1, true)
        );
        assert_eq!(desugar(&expr).unwrap(), expected);
        
        // !(e1 & e2) = !e1 + !e2
        let e1 = Expr::test(0, true);
        let e2 = Expr::test(1, false);
        let expr = Expr::TestNegation(Expr::intersect(e1, e2));
        let expected = Expr::union(
            Expr::test(0, false),
            Expr::test(1, true)
        );
        assert_eq!(desugar(&expr).unwrap(), expected);
    }
    
    #[test]
    fn test_desugar_invalid_test_negation() {
        // Test that TestNegation on non-test fragment gives error
        
        // !T is invalid
        let expr = Expr::TestNegation(Expr::top());
        assert!(desugar(&expr).is_err());
        
        // !(x0 := 1) is invalid
        let expr = Expr::TestNegation(Expr::assign(0, true));
        assert!(desugar(&expr).is_err());
        
        // !dup is invalid
        let expr = Expr::TestNegation(Expr::dup());
        assert!(desugar(&expr).is_err());
        
        // !(e*) is invalid
        let expr = Expr::TestNegation(Expr::star(Expr::zero()));
        assert!(desugar(&expr).is_err());
    }
    
    #[test]
    fn test_desugar_double_negation() {
        // !!e = e
        let e = Expr::test(0, true);
        let expr = Expr::TestNegation(Expr::test_negation(e.clone()));
        assert_eq!(desugar(&expr).unwrap(), e);
    }
    
    #[test]
    fn test_desugar_if_then_else() {
        // if (x0 == 1) then (x1 := 1) else (x1 := 0)
        // Should become: (x0 == 1 ; x1 := 1) + (x0 == 0 ; x1 := 0)
        let cond = Expr::test(0, true);
        let then_expr = Expr::assign(1, true);
        let else_expr = Expr::assign(1, false);
        let if_expr = Expr::if_then_else(cond.clone(), then_expr.clone(), else_expr.clone());
        
        let expected = Expr::union(
            Expr::sequence(Expr::test(0, true), Expr::assign(1, true)),
            Expr::sequence(Expr::test(0, false), Expr::assign(1, false))
        );
        
        assert_eq!(desugar(&if_expr).unwrap(), expected);
    }
    
    #[test] 
    fn test_desugar_if_then_else_invalid_condition() {
        // if (x0 := 1) then 1 else 0 - invalid because condition is not test fragment
        let cond = Expr::assign(0, true);
        let if_expr = Expr::if_then_else(cond, Expr::one(), Expr::zero());
        
        assert!(desugar(&if_expr).is_err());
    }
    
    #[test]
    fn test_desugar_let_simple() {
        // let a = (x0 == 1) in a + (x1 == 0)
        // Should become: (x0 == 1) + (x1 == 0)
        let def = Expr::test(0, true);
        let body = Expr::union(Expr::var("a".to_string()), Expr::test(1, false));
        let let_expr = Expr::let_in("a".to_string(), def.clone(), body);
        
        let expected = Expr::union(Expr::test(0, true), Expr::test(1, false));
        assert_eq!(desugar(&let_expr).unwrap(), expected);
    }
    
    #[test]
    fn test_desugar_let_nested() {
        // let a = (x0 == 1) in let b = (x1 == 0) in a + b
        // Should become: (x0 == 1) + (x1 == 0)
        let a_def = Expr::test(0, true);
        let b_def = Expr::test(1, false);
        let inner_body = Expr::union(Expr::var("a".to_string()), Expr::var("b".to_string()));
        let inner_let = Expr::let_in("b".to_string(), b_def, inner_body);
        let outer_let = Expr::let_in("a".to_string(), a_def, inner_let);
        
        let expected = Expr::union(Expr::test(0, true), Expr::test(1, false));
        assert_eq!(desugar(&outer_let).unwrap(), expected);
    }
    
    #[test]
    fn test_desugar_let_shadowing() {
        // let a = (x0 == 1) in let a = (x1 == 0) in a
        // Should become: (x1 == 0) (inner a shadows outer a)
        let outer_def = Expr::test(0, true);
        let inner_def = Expr::test(1, false);
        let body = Expr::var("a".to_string());
        let inner_let = Expr::let_in("a".to_string(), inner_def.clone(), body);
        let outer_let = Expr::let_in("a".to_string(), outer_def, inner_let);
        
        let expected = Expr::test(1, false);
        assert_eq!(desugar(&outer_let).unwrap(), expected);
    }
    
    #[test]
    fn test_desugar_unbound_variable() {
        // Just a variable with no binding
        let var_expr = Expr::var("unbound".to_string());
        assert!(desugar(&var_expr).is_err());
    }
    
    #[test]
    fn test_desugar_let_with_complex_expr() {
        // let p = (x0 == 1 ; x1 := 0) in p + p*
        let def = Expr::sequence(Expr::test(0, true), Expr::assign(1, false));
        let body = Expr::union(Expr::var("p".to_string()), Expr::star(Expr::var("p".to_string())));
        let let_expr = Expr::let_in("p".to_string(), def.clone(), body);
        
        let expected = Expr::union(
            Expr::sequence(Expr::test(0, true), Expr::assign(1, false)),
            Expr::star(Expr::sequence(Expr::test(0, true), Expr::assign(1, false)))
        );
        assert_eq!(desugar(&let_expr).unwrap(), expected);
    }
    
    #[test]
    fn test_bit_range_assign() {
        // x[0..3] := 5 (binary: 101)
        let expr = Expr::bit_range_assign(0, 3, vec![true, false, true]);
        let expected = Expr::sequence(
            Expr::assign(0, true),
            Expr::sequence(
                Expr::assign(1, false),
                Expr::assign(2, true)
            )
        );
        assert_eq!(desugar(&expr).unwrap(), expected);
    }
    
    #[test]
    fn test_bit_range_test() {
        // x[2..5] == 6 (binary: 110)
        let expr = Expr::bit_range_test(2, 5, vec![false, true, true]);
        let expected = Expr::intersect(
            Expr::test(2, false),
            Expr::intersect(
                Expr::test(3, true),
                Expr::test(4, true)
            )
        );
        assert_eq!(desugar(&expr).unwrap(), expected);
    }
    
    #[test]
    fn test_bit_range_empty() {
        // Empty ranges should desugar to 1
        let assign = Expr::bit_range_assign(5, 5, vec![]);
        assert_eq!(desugar(&assign).unwrap(), Expr::one());
        
        let test = Expr::bit_range_test(5, 5, vec![]);
        assert_eq!(desugar(&test).unwrap(), Expr::one());
    }
    
    #[test]
    fn test_bit_range_single() {
        // Single bit ranges
        let assign = Expr::bit_range_assign(10, 11, vec![true]);
        assert_eq!(desugar(&assign).unwrap(), Expr::assign(10, true));
        
        let test = Expr::bit_range_test(10, 11, vec![false]);
        assert_eq!(desugar(&test).unwrap(), Expr::test(10, false));
    }
}