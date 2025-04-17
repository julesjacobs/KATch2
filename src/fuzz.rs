use crate::expr::{Exp, Expr};
use crate::pre::{Field, Value};
use rand::Rng; // Use Rng trait directly

// --- Random Expression Generation ---

// Note: Generic over R: Rng to fix 'dyn Rng' errors
fn gen_random_field(k: u32) -> Field {
    // k must be > 0 for this to be called meaningfully
    if k == 0 {
        panic!("Cannot generate field with k=0");
    }
    rand::random_range(0..k)
}

fn gen_random_value() -> Value {
    rand::random::<bool>()
}

// Generates a random expression.
fn gen_random_expr(num_fields: u32, max_depth: usize) -> Exp {
    // Base case: terminals or depth limit reached
    if max_depth == 0 {
        match rand::random_range(0..5) {
            0 => Expr::zero(),
            1 => Expr::one(),
            2 => Expr::top(),
            3 => Expr::dup(),
            4 => {
                // Assign (only reachable if k > 0)
                Expr::assign(gen_random_field(num_fields), gen_random_value())
            }
            5 => {
                // Test (only reachable if k > 0)
                Expr::test(gen_random_field(num_fields), gen_random_value())
            }
            _ => unreachable!(),
        }
    } else {
        match rand::random_range(0..6) {
            0 => gen_random_expr(num_fields, max_depth - 1),
            1 => Expr::star(gen_random_expr(num_fields, max_depth - 1)),
            2 => Expr::complement(gen_random_expr(num_fields, max_depth - 1)),
            3 => Expr::union(
                gen_random_expr(num_fields, max_depth - 1),
                gen_random_expr(num_fields, max_depth - 1),
            ),
            4 => Expr::sequence(
                gen_random_expr(num_fields, max_depth - 1),
                gen_random_expr(num_fields, max_depth - 1),
            ),
            5 => Expr::intersect(
                gen_random_expr(num_fields, max_depth - 1),
                gen_random_expr(num_fields, max_depth - 1),
            ),
            _ => unreachable!(),
        }
    }
}

/// Gets two distinct fields that are each in the range [0, k].
/// Panics if k < 2. Should be guarded before calling.
fn get_distinct_fields(k: u32) -> (Field, Field) {
    if k < 2 {
        panic!("get_distinct_fields called with k < 2");
    }
    let f1 = rand::random_range(0..k);
    let mut f2 = rand::random_range(0..k);
    while f1 == f2 {
        f2 = rand::random_range(0..k);
    }
    (f1, f2)
}

// --- Main Fuzzing Function ---

// The `genax(n, d, k)` function returns a random pair of semantically equivalent expressions.
// The n parameter controls how many axioms are used.
// The d parameter controls the depth of each expression (viewing the expr as an AST).
// The k parameter controls how many variables are used.
// The function works as follows:
// - For genax(0, d, k), return a pair (e, e) where e is a random expression of depth d with k variables.
// - For genax(n, d, k), pick any axiom, say e1 + e2 = e2 + e1.
//   Then recursively generate (e1, e1') and (e2, e2') using genax(n-1, d, k) and
//   substitute them into the axiom and return (e1 + e2, e2' + e1').

// Here is the list of axioms we can pick from:

// # NetKAT Axioms for Binary Variables

// **Notation:**
// *   `p, q, r, a, b, c`: Arbitrary NetKAT policies.
// *   `xi, xj`: Distinct binary variable field names (where `i` and `j` are distinct non-negative integers).
// *   `v, v'`: Binary values (either `0` or `1`).
// *   `¬a`: Negation of predicate `a`.
// *   `0`: The policy/predicate that drops/rejects all packets (false).
// *   `1`: The policy/predicate that passes/accepts all packets unmodified (true).
// *   `+`: Union / Disjunction.
// *   `.`: Sequential Composition / Conjunction.
// *   `*`: Kleene Star (iteration).
// *   `xi = v`: Test if field `xi` equals value `v`.
// *   `xi <- v`: Modification setting field `xi` to value `v`.
// *   `dup`: Duplication policy (records current packet state in history).

// *   `p + (q + r) = (p + q) + r` *(KA-PLUS-ASSOC)*
// *   `p + q = q + p` *(KA-PLUS-COMM)*
// *   `p + 0 = p` *(KA-PLUS-ZERO)*
// *   `p + p = p` *(KA-PLUS-IDEM)*
// *   `p . (q . r) = (p . q) . r` *(KA-SEQ-ASSOC)*
// *   `1 . p = p` *(KA-ONE-SEQ - Left)*
// *   `p . 1 = p` *(KA-SEQ-ONE - Right)*
// *   `p . (q + r) = p . q + p . r` *(KA-SEQ-DIST-L)*
// *   `(p + q) . r = p . r + q . r` *(KA-SEQ-DIST-R)*
// *   `0 . p = 0` *(KA-ZERO-SEQ - Left)*
// *   `p . 0 = 0` *(KA-SEQ-ZERO - Right)*
// *   `1 + p . p* = p*` *(KA-UNROLL-L - Left)*
// *   `1 + p* . p = p*` *(KA-UNROLL-R - Right)*
// *   `a + (b & c) = (a + b) & (a + c)` *(BA-PLUS-DIST)*
// *   `a + T = T` *(BA-PLUS-ONE)*
// *   `a + ¬a = T` *(BA-EXCL-MID)*
// *   `a & b = b & a` *(BA-SEQ-COMM)*
// *   `a & ¬a = 0` *(BA-CONTRA)*
// *   `a & a = a` *(BA-SEQ-IDEM)*
// *   `xi <- v . xj <- v' = xj <- v' . xi <- v` *(PA-MOD-MOD-COMM)* -- Modifications on distinct variables commute
// *   `(xi <- v) . (xj = v') = (xj = v') . (xi <- v)` *(PA-MOD-FILTER-COMM)* -- Modification on `xi` commutes with a test on distinct variable `xj`
// *   `dup . (xi = v) = (xi = v) . dup` *(PA-DUP-FILTER-COMM)* -- Dup commutes with tests
// *   `(xi <- v) . (xi = v) = xi <- v` *(PA-MOD-FILTER)* -- Testing for a value immediately after setting it is redundant
// *   `(xi = v) . (xi <- v) = (xi = v)` *(PA-FILTER-MOD)* -- Setting a variable to a value it's already known to have doesn't change the test result
// *   `(xi <- v) . (xi <- v') = xi <- v'` *(PA-MOD-MOD)* -- Sequential modifications to the same variable; the last one prevails
// *   `(xi = 0) . (xi = 1) = 0` *(PA-CONTRA) -- A variable cannot be both 0 and 1 simultaneously
// *   `(xi = 0) + (xi = 1) = 1` *(PA-MATCH-ALL) -- A binary variable must be either 0 or 1

// The following axioms are not used in the current implementation.
// *   `q + p . r <= r => p* . q <= r` *(KA-LFP-L - Left Induction)* (UNUSED)
// *   `p + q . r <= q => p . r* <= q` *(KA-LFP-R - Right Induction)* (UNUSED)
//     *(Note: `x <= y` is shorthand for `x + y = y`)*

/// Generates a pair of semantically equivalent expressions.
///
/// - `n` (`ax_depth`): Controls the number of axiom applications (recursion depth).
/// - `d` (`expr_depth`): Controls the depth of the generated expression
/// - `k` (`num_fields`): Controls the maximum number of distinct variables (fields `x0` to `xk-1`).
pub fn genax(ax_depth: usize, expr_depth: usize, num_fields: u32) -> (Exp, Exp) {
    assert!(
        num_fields >= 2,
        "num_fields must be >= 2 to generate distinct fields"
    );
    if ax_depth == 0 {
        // Base case: return (e, e) where e is a random expression
        let random_expr = gen_random_expr(num_fields, expr_depth); // Use a default depth
        return (random_expr.clone(), random_expr);
    }
    // Recursive step: pick an axiom and apply it
    match rand::random_range(0..4) {
        // Number of recursive calls
        0 => {
            // --- PA Axioms --- (No recursive calls needed)
            match rand::random_range(0..8) {
                0 => {
                    // PA-MOD-MOD-COMM: `xi <- v . xj <- v' = xj <- v' . xi <- v`
                    let (xi, xj) = get_distinct_fields(num_fields);
                    let v = gen_random_value();
                    let v_prime = gen_random_value();
                    return (
                        Expr::sequence(Expr::assign(xi, v), Expr::assign(xj, v_prime)),
                        Expr::sequence(Expr::assign(xj, v_prime), Expr::assign(xi, v)),
                    );
                }
                1 => {
                    // PA-MOD-FILTER-COMM
                    let (xi, xj) = get_distinct_fields(num_fields);
                    let v = gen_random_value();
                    let v_prime = gen_random_value();
                    return (
                        Expr::sequence(Expr::assign(xi, v), Expr::test(xj, v_prime)),
                        Expr::sequence(Expr::test(xj, v_prime), Expr::assign(xi, v)),
                    );
                }
                2 => {
                    // PA-DUP-FILTER-COMM
                    let xi = gen_random_field(num_fields);
                    let v = gen_random_value();
                    return (
                        Expr::sequence(Expr::dup(), Expr::test(xi, v)),
                        Expr::sequence(Expr::test(xi, v), Expr::dup()),
                    );
                }
                3 => {
                    // PA-MOD-FILTER
                    let xi = gen_random_field(num_fields);
                    let v = gen_random_value();
                    return (
                        Expr::sequence(Expr::assign(xi, v), Expr::test(xi, v)),
                        Expr::assign(xi, v),
                    );
                }
                4 => {
                    // PA-FILTER-MOD
                    let xi = gen_random_field(num_fields);
                    let v = gen_random_value();
                    return (
                        Expr::sequence(Expr::test(xi, v), Expr::assign(xi, v)),
                        Expr::test(xi, v),
                    );
                }
                5 => {
                    // PA-MOD-MOD: `(xi <- v) . (xi <- v') = xi <- v'`
                    let xi = gen_random_field(num_fields);
                    let v = gen_random_value();
                    let v_prime = gen_random_value();
                    return (
                        Expr::sequence(Expr::assign(xi, v), Expr::assign(xi, v_prime)),
                        Expr::assign(xi, v_prime),
                    );
                }
                6 => {
                    // PA-CONTRA: `(xi = 0) . (xi = 1) = 0`
                    let xi = gen_random_field(num_fields);
                    return (
                        Expr::sequence(Expr::test(xi, false), Expr::test(xi, true)),
                        Expr::zero(),
                    );
                }
                7 => {
                    // PA-MATCH-ALL: `(xi = 0) + (xi = 1) = 1`
                    let xi = gen_random_field(num_fields);
                    return (
                        Expr::union(Expr::test(xi, false), Expr::test(xi, true)),
                        Expr::one(),
                    );
                }
                _ => unreachable!(),
            }
        }
        1 => {
            let (lhs, rhs) = genax(ax_depth - 1, expr_depth, num_fields);
            match rand::random_range(0..17) {
                0 => {
                    // KA-PLUS-ZERO: p + 0 = p
                    return (Expr::union(lhs, Expr::zero()), rhs);
                }
                1 => {
                    // KA-PLUS-IDEM: p + p = p
                    return (Expr::union(lhs.clone(), lhs), rhs);
                }
                2 => {
                    // KA-ONE-SEQ: 1 . p = p
                    return (Expr::sequence(Expr::one(), lhs), rhs);
                }
                3 => {
                    // KA-SEQ-ONE: p . 1 = p
                    return (Expr::sequence(lhs, Expr::one()), rhs);
                }
                4 => {
                    // KA-ZERO-SEQ: 0 . p = 0
                    return (Expr::sequence(Expr::zero(), lhs), Expr::zero()); // rhs unused
                }
                5 => {
                    // KA-SEQ-ZERO: p . 0 = 0
                    return (Expr::sequence(lhs, Expr::zero()), Expr::zero()); // rhs unused
                }
                6 => {
                    // KA-UNROLL-L: 1 + p . p* = p*
                    return (
                        Expr::union(Expr::one(), Expr::sequence(lhs.clone(), Expr::star(lhs))),
                        Expr::star(rhs),
                    );
                }
                7 => {
                    // KA-UNROLL-R: 1 + p* . p = p*
                    return (
                        Expr::union(Expr::one(), Expr::sequence(Expr::star(lhs.clone()), lhs)),
                        Expr::star(rhs),
                    );
                }
                8 => {
                    // BA-PLUS-ONE: a + T = T
                    return (Expr::union(lhs, Expr::top()), Expr::top()); // rhs unused
                }
                9 => {
                    // BA-EXCL-MID: a + ¬a = T
                    return (Expr::union(lhs, Expr::complement(rhs)), Expr::top());
                }
                10 => {
                    // BA-CONTRA: a & ¬a = 0
                    return (Expr::intersect(lhs, Expr::complement(rhs)), Expr::zero());
                }
                11 => {
                    // BA-SEQ-IDEM: a & a = a
                    return (Expr::intersect(lhs.clone(), lhs), rhs);
                }
                12 => {
                    // !(F e) = G (!e)
                    let new_lhs = Expr::complement(Expr::ltl_finally(lhs));
                    let new_rhs = Expr::ltl_globally(Expr::complement(rhs));
                    return (new_lhs, new_rhs);
                }
                13 => {
                    // !(G e) = F (!e)
                    let new_lhs = Expr::complement(Expr::ltl_globally(lhs));
                    let new_rhs = Expr::ltl_finally(Expr::complement(rhs));
                    return (new_lhs, new_rhs);
                }
                14 => {
                    // !(X e) = End + X (!e)
                    let new_lhs = Expr::complement(Expr::ltl_next(lhs));
                    let new_rhs = Expr::union(Expr::end(), Expr::ltl_next(Expr::complement(rhs)));
                    return (new_lhs, new_rhs);
                }
                15 => {
                    // F e = e + X (F e)
                    let new_lhs = Expr::ltl_finally(lhs);
                    let new_rhs = Expr::union(rhs.clone(), Expr::ltl_next(Expr::ltl_finally(rhs)));
                    return (new_lhs, new_rhs);
                }
                16 => {
                    // G e = e & (End + X (G e))
                    let new_lhs = Expr::ltl_globally(lhs);
                    let new_rhs = Expr::intersect(
                        rhs.clone(),
                        Expr::union(Expr::end(), Expr::ltl_next(Expr::ltl_globally(rhs))),
                    );
                    return (new_lhs, new_rhs);
                }
                _ => unreachable!(),
            }
        }
        2 => {
            let (p1_lhs, p1_rhs) = genax(ax_depth - 1, expr_depth, num_fields);
            let (p2_lhs, p2_rhs) = genax(ax_depth - 1, expr_depth, num_fields);
            match rand::random_range(0..5) {
                0 => {
                    // KA-PLUS-COMM: p + q = q + p
                    return (Expr::union(p1_lhs, p2_lhs), Expr::union(p2_rhs, p1_rhs));
                }
                1 => {
                    // BA-SEQ-COMM: a & b = b & a
                    return (
                        Expr::intersect(p1_lhs, p2_lhs),
                        Expr::intersect(p2_rhs, p1_rhs),
                    );
                }
                2 => {
                    // X (e1 & e2) = X e1 & X e2
                    let new_lhs = Expr::ltl_next(Expr::intersect(p1_lhs, p2_lhs));
                    let new_rhs = Expr::intersect(Expr::ltl_next(p1_rhs), Expr::ltl_next(p2_rhs));
                    return (new_lhs, new_rhs);
                }
                3 => {
                    // X (e1 + e2) = X e1 + X e2
                    let new_lhs = Expr::ltl_next(Expr::union(p1_lhs, p2_lhs));
                    let new_rhs = Expr::union(Expr::ltl_next(p1_rhs), Expr::ltl_next(p2_rhs));
                    return (new_lhs, new_rhs);
                }
                4 => {
                    // e1 U e2 = e2 + (e1 & X (e1 U e2))
                    let new_lhs = Expr::ltl_until(p1_lhs, p2_lhs);
                    let new_rhs = Expr::union(
                        p2_rhs.clone(),
                        Expr::intersect(
                            p1_rhs.clone(),
                            Expr::ltl_next(Expr::ltl_until(p1_rhs, p2_rhs)),
                        ),
                    );
                    return (new_lhs, new_rhs);
                }
                _ => unreachable!(),
            }
        }
        3 => {
            let (p1_lhs, p1_rhs) = genax(ax_depth - 1, expr_depth, num_fields);
            let (p2_lhs, p2_rhs) = genax(ax_depth - 1, expr_depth, num_fields);
            let (p3_lhs, p3_rhs) = genax(ax_depth - 1, expr_depth, num_fields);
            match rand::random_range(0..5) {
                0 => {
                    // KA-PLUS-ASSOC: p + (q + r) = (p + q) + r
                    return (
                        Expr::union(p1_lhs, Expr::union(p2_lhs, p3_lhs)),
                        Expr::union(Expr::union(p1_rhs, p2_rhs), p3_rhs),
                    );
                }
                1 => {
                    // KA-SEQ-ASSOC: p . (q . r) = (p . q) . r
                    return (
                        Expr::sequence(p1_lhs, Expr::sequence(p2_lhs, p3_lhs)),
                        Expr::sequence(Expr::sequence(p1_rhs, p2_rhs), p3_rhs),
                    );
                }
                2 => {
                    // KA-SEQ-DIST-L: p . (q + r) = p . q + p . r
                    return (
                        Expr::sequence(p1_lhs.clone(), Expr::union(p2_lhs, p3_lhs)),
                        Expr::union(
                            Expr::sequence(p1_rhs.clone(), p2_rhs),
                            Expr::sequence(p1_rhs, p3_rhs),
                        ),
                    );
                }
                3 => {
                    // KA-SEQ-DIST-R: (p + q) . r = p . r + q . r
                    return (
                        Expr::sequence(Expr::union(p1_lhs, p2_lhs), p3_lhs.clone()),
                        Expr::union(
                            Expr::sequence(p1_rhs, p3_rhs.clone()),
                            Expr::sequence(p2_rhs, p3_rhs),
                        ),
                    );
                }
                4 => {
                    // BA-PLUS-DIST: a + (b & c) = (a + b) & (a + c)
                    return (
                        Expr::union(p1_lhs.clone(), Expr::intersect(p2_lhs, p3_lhs)),
                        Expr::intersect(
                            Expr::union(p1_rhs.clone(), p2_rhs),
                            Expr::union(p1_rhs, p3_rhs),
                        ),
                    );
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

/// Shrinks an expression, returning a list of subterms:
/// - `f == v, f := v, Top` are shrunken to the list `[0, 1]`
/// - Binary operators `e1 ⊙ e2` are shrunken to the list `[e1, e2]`
/// - Unary operator, e.g. `!e` are shrunken to the list `[e]`
fn shrink_exp(exp: Exp) -> Vec<Exp> {
    use Expr::*;
    match *exp {
        Zero | One => vec![],
        Top | Dup | End | Assign(_, _) | Test(_, _) => vec![Expr::zero(), Expr::one()],
        Union(e1, e2)
        | Intersect(e1, e2)
        | Xor(e1, e2)
        | Difference(e1, e2)
        | Sequence(e1, e2)
        | LtlUntil(e1, e2) => {
            vec![e1, e2]
        }
        Star(e) | Complement(e) | LtlNext(e) => vec![e],
    }
}

#[cfg(test)]
mod tests {
    use crate::aut::Aut;

    use super::*;
    use rand::rngs::StdRng;
    use rand::SeedableRng;

    #[test]
    fn print_random_genax() {
        let ax_depth = 2;
        let expr_depth = 0;
        let num_fields = 3;
        let number = 100;
        for _ in 0..number {
            let (e1, e2) = genax(ax_depth, expr_depth, num_fields);
            println!("  {}\n   ===\n  {}\n", e1, e2);
        }
    }

    #[test]
    fn fuzz_test() {
        // Enable backtrace for debugging failing tests
        unsafe {
            std::env::set_var("RUST_BACKTRACE", "1");
        }

        // Generate random expressions, create the xor, and check if the automaton is empty
        let ax_depth = 3;
        let expr_depth = 1;
        let num_fields = 3;
        let number = 10000;
        for _ in 0..number {
            let (e1, e2) = genax(ax_depth, expr_depth, num_fields);
            println!("Checking xor of\n  {}\n   ===\n  {}\n", e1, e2);
            let xor = Expr::xor(e1.clone(), e2.clone());
            println!("XOR result = {}\n", xor);
            let mut aut = Aut::new(num_fields);
            let state = aut.expr_to_state(&xor);
            if aut.is_empty(state) {
                println!("Success!\n");
            } else {
                assert!(false, "Failure!\n");
            }
        }
    }
}
