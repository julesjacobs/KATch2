use crate::expr::{Exp, Expr};
use crate::pre::{Field, Value};
use rand::Rng; // Use Rng trait directly

// --- Random Expression Generation ---

// Note: Generic over R: Rng to fix 'dyn Rng' errors

/// Generates a random field value in the range [0..k]
fn gen_random_field(k: u32) -> Field {
    // k must be > 0 for this to be called meaningfully
    if k == 0 {
        panic!("Cannot generate field with k=0");
    }
    rand::random_range(0..k)
}

/// Generates a random boolean (binary field)
fn gen_random_value() -> Value {
    rand::random::<bool>()
}

/// Generates a random expression
fn gen_random_expr(num_fields: u32, max_depth: usize) -> Exp {
    // Base case: terminals or depth limit reached
    if max_depth == 0 {
        match rand::random_range(0..7) {
            0 => Expr::zero(),
            1 => Expr::one(),
            2 => Expr::top(),
            3 => Expr::dup(),
            4 => {
                // Assign (only reachable if num_fields > 0)
                Expr::assign(gen_random_field(num_fields), gen_random_value())
            }
            5 => {
                // Test (only reachable if num_fields > 0)
                Expr::test(gen_random_field(num_fields), gen_random_value())
            }
            6 => Expr::end(),
            _ => unreachable!(),
        }
    } else {
        match rand::random_range(0..15) {
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
            6 => Expr::ltl_next(gen_random_expr(num_fields, max_depth - 1)),
            7 => Expr::ltl_weak_next(gen_random_expr(num_fields, max_depth - 1)),
            8 => Expr::ltl_until(
                gen_random_expr(num_fields, max_depth - 1),
                gen_random_expr(num_fields, max_depth - 1),
            ),
            9 => Expr::ltl_weak_until(
                gen_random_expr(num_fields, max_depth - 1),
                gen_random_expr(num_fields, max_depth - 1),
            ),
            10 => Expr::ltl_finally(gen_random_expr(num_fields, max_depth - 1)),
            11 => Expr::ltl_globally(gen_random_expr(num_fields, max_depth - 1)),
            12 => Expr::ltl_release(
                gen_random_expr(num_fields, max_depth - 1),
                gen_random_expr(num_fields, max_depth - 1),
            ),
            13 => Expr::ltl_strong_release(
                gen_random_expr(num_fields, max_depth - 1),
                gen_random_expr(num_fields, max_depth - 1),
            ),
            14 => Expr::xor(
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

/// Flips a coin to decide whether to swap the LHS and RHS of an equality.    
/// (Helper function used in `genax` below)
fn flip_equality_rand(lhs: Exp, rhs: Exp) -> (Exp, Exp) {
    let b = rand::random::<bool>();
    if b {
        (rhs, lhs)
    } else {
        (lhs, rhs)
    }
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
//   substitute them into the axiom and return (e2' + e1', e1 + e2).
//   Note: we randomly swap the lhs & rhs with each recursive call (using `flip_equality_rand` above)
//   so that equations are applied in both orders.

// Here is the list of axioms we can pick from:

// # NetKAT Axioms for Binary Variables

// **Notation:**
// *   `p, q, r, a, b, c`: Arbitrary NetKAT policies.
// *   `xi, xj`: Distinct binary variable field names (where `i` and `j` are distinct non-negative integers).
// *   `v, v'`: Binary values (either `0` or `1`).
// *   `¬a`: Negation of predicate `a`.
// *   `0`: The policy/predicate that drops/rejects all packets (false).
// *   `1`: The policy/predicate that passes/accepts all packets unmodified (true).
// *   `+`: Expr::union / Disjunction.
// *   `.`: Sequential Composition / Conjunction.
// *   `*`: Kleene Expr::star (iteration).
// *   `xi = v`: Expr::test if field `xi` equals value `v`.
// *   `xi <- v`: Modification setting field `xi` to value `v`.
// *   `dup`: Expr::dup()lication policy (records current packet state in history).

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
// *    LTL equivalences (see corresponding write-up for full list)

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
            match rand::random_range(0..9) {
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
                8 => {
                    // ⊤ = End; (Dup; End)*
                    let lhs = Expr::top();
                    let rhs = Expr::sequence(
                        Expr::end(),
                        Expr::star(Expr::sequence(Expr::dup(), Expr::end())),
                    );
                    return (lhs, rhs);
                }
                _ => unreachable!(),
            }
        }
        1 => {
            let (lhs, rhs) = genax(ax_depth - 1, expr_depth, num_fields);
            match rand::random_range(0..21) {
                0 => {
                    // KA-PLUS-ZERO: p + 0 = p
                    let new_lhs = Expr::union(lhs, Expr::zero());
                    let new_rhs = rhs;
                    return flip_equality_rand(new_lhs, new_rhs); // Swap rhs & lhs
                }
                1 => {
                    // KA-PLUS-IDEM: p + p = p
                    let new_lhs = Expr::union(lhs.clone(), lhs);
                    let new_rhs = rhs;
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                2 => {
                    // KA-ONE-SEQ: 1 . p = p
                    let new_lhs = Expr::sequence(Expr::one(), lhs);
                    let new_rhs = rhs;
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                3 => {
                    // KA-SEQ-ONE: p . 1 = p
                    let new_lhs = Expr::sequence(lhs, Expr::one());
                    let new_rhs = rhs;
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                4 => {
                    // KA-ZERO-SEQ: 0 . p = 0
                    let new_lhs = Expr::sequence(Expr::zero(), lhs);
                    let new_rhs = Expr::zero(); // rhs unused
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                5 => {
                    // KA-SEQ-ZERO: p . 0 = 0
                    let new_lhs = Expr::sequence(lhs, Expr::zero());
                    let new_rhs = Expr::zero(); // rhs unused
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                6 => {
                    // KA-UNROLL-L: 1 + p . p* = p*
                    let new_lhs =
                        Expr::union(Expr::one(), Expr::sequence(lhs.clone(), Expr::star(lhs)));
                    let new_rhs = Expr::star(rhs);
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                7 => {
                    // KA-UNROLL-R: 1 + p* . p = p*
                    let new_lhs =
                        Expr::union(Expr::one(), Expr::sequence(Expr::star(lhs.clone()), lhs));
                    let new_rhs = Expr::star(rhs);
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                8 => {
                    // BA-PLUS-ONE: a + T = T
                    let new_lhs = Expr::union(lhs, Expr::top());
                    let new_rhs = Expr::top(); // rhs unused
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                9 => {
                    // BA-EXCL-MID: a + ¬a = T
                    let new_lhs = Expr::union(lhs, Expr::complement(rhs));
                    let new_rhs = Expr::top();
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                10 => {
                    // BA-CONTRA: a & ¬a = 0
                    let new_lhs = Expr::intersect(lhs, Expr::complement(rhs));
                    let new_rhs = Expr::zero();
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                11 => {
                    // BA-SEQ-IDEM: a & a = a
                    let new_lhs = Expr::intersect(lhs.clone(), lhs);
                    let new_rhs = rhs;
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                12 => {
                    // !(F e) = G (!e)
                    let new_lhs = Expr::complement(Expr::ltl_finally(lhs));
                    let new_rhs = Expr::ltl_globally(Expr::complement(rhs));
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                13 => {
                    // !(G e) = F (!e)
                    let new_lhs = Expr::complement(Expr::ltl_globally(lhs));
                    let new_rhs = Expr::ltl_finally(Expr::complement(rhs));
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                14 => {
                    // !(X e) = End + X (!e)
                    let new_lhs = Expr::complement(Expr::ltl_next(lhs));
                    let new_rhs = Expr::union(Expr::end(), Expr::ltl_next(Expr::complement(rhs)));
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                15 => {
                    // F e = e + X (F e)
                    let new_lhs = Expr::ltl_finally(lhs);
                    let new_rhs = Expr::union(rhs.clone(), Expr::ltl_next(Expr::ltl_finally(rhs)));
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                16 => {
                    // G e = e & (End + X (G e)) = X' e
                    let new_lhs = Expr::ltl_globally(lhs);
                    let new_rhs = Expr::intersect(
                        rhs.clone(),
                        Expr::union(Expr::end(), Expr::ltl_next(Expr::ltl_globally(rhs))),
                    );
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                17 => {
                    // F (F e) = F e
                    let new_lhs = Expr::ltl_finally(Expr::ltl_finally(lhs));
                    let new_rhs = Expr::ltl_finally(rhs);
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                18 => {
                    // G (G e) = G e
                    let new_lhs = Expr::ltl_globally(Expr::ltl_globally(lhs));
                    let new_rhs = Expr::ltl_globally(rhs);
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                19 => {
                    // G (F (G e) = F (G e)
                    let new_lhs = Expr::ltl_globally(Expr::ltl_finally(Expr::ltl_globally(lhs)));
                    let new_rhs = Expr::ltl_finally(Expr::ltl_globally(rhs));
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                20 => {
                    // F (G (F e)) = G (F e)
                    let new_lhs = Expr::ltl_finally(Expr::ltl_globally(Expr::ltl_finally(lhs)));
                    let new_rhs = Expr::ltl_globally(Expr::ltl_finally(rhs));
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                _ => unreachable!(),
            }
        }
        2 => {
            let (p1_lhs, p1_rhs) = genax(ax_depth - 1, expr_depth, num_fields);
            let (p2_lhs, p2_rhs) = genax(ax_depth - 1, expr_depth, num_fields);
            match rand::random_range(0..18) {
                0 => {
                    // KA-PLUS-COMM: p + q = q + p
                    let new_lhs = Expr::union(p1_lhs, p2_lhs);
                    let new_rhs = Expr::union(p2_rhs, p1_rhs);
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                1 => {
                    // BA-SEQ-COMM: a & b = b & a
                    let new_lhs = Expr::intersect(p1_lhs, p2_lhs);
                    let new_rhs = Expr::intersect(p2_rhs, p1_rhs);
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                2 => {
                    // X (e1 & e2) = X e1 & X e2
                    let new_lhs = Expr::ltl_next(Expr::intersect(p1_lhs, p2_lhs));
                    let new_rhs = Expr::intersect(Expr::ltl_next(p1_rhs), Expr::ltl_next(p2_rhs));
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                3 => {
                    // X (e1 + e2) = X e1 + X e2
                    let new_lhs = Expr::ltl_next(Expr::union(p1_lhs, p2_lhs));
                    let new_rhs = Expr::union(Expr::ltl_next(p1_rhs), Expr::ltl_next(p2_rhs));
                    return flip_equality_rand(new_lhs, new_rhs);
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
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                5 => {
                    // e1 W e2 = e2 + (e1 & X' (e1 W e2))
                    // Weak Until expansion law - with weak next instead of next
                    let new_lhs = Expr::ltl_weak_until(p1_lhs, p2_lhs);
                    let new_rhs = Expr::union(
                        p2_rhs.clone(),
                        Expr::intersect(
                            p1_rhs.clone(),
                            Expr::ltl_weak_next(Expr::ltl_weak_until(p1_rhs, p2_rhs)),
                        ),
                    );
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                6 => {
                    // !(e1 U e2) = !e1 R !e2
                    let new_lhs = Expr::complement(Expr::ltl_until(p1_lhs, p2_lhs));
                    let new_rhs =
                        Expr::ltl_release(Expr::complement(p1_rhs), Expr::complement(p2_rhs));
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                7 => {
                    // e1 R e2 = e2 & (e1 + X' (e1 R e2))
                    let new_lhs = Expr::ltl_release(p1_lhs, p2_lhs);
                    let new_rhs = Expr::intersect(
                        p2_rhs.clone(),
                        Expr::union(
                            p1_rhs.clone(),
                            Expr::ltl_weak_next(Expr::ltl_release(p1_rhs, p2_rhs)),
                        ),
                    );
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                8 => {
                    // !(e1 R e2) = !e1 U !e2
                    let new_lhs = Expr::complement(Expr::ltl_release(p1_lhs, p2_lhs));
                    let new_rhs =
                        Expr::ltl_until(Expr::complement(p1_rhs), Expr::complement(p2_rhs));
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                9 => {
                    // e1 M e2 = e2 & (e1 + X (e1 M e2))
                    let new_lhs = Expr::ltl_strong_release(p1_lhs, p2_lhs);

                    let new_rhs = Expr::intersect(
                        p2_rhs.clone(),
                        Expr::union(
                            p1_rhs.clone(),
                            Expr::ltl_next(Expr::ltl_strong_release(p1_rhs, p2_rhs)),
                        ),
                    );
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                10 => {
                    // !(e1 M e2) = !e1 W !e2
                    let new_lhs = Expr::complement(Expr::ltl_strong_release(p1_lhs, p2_lhs));
                    let new_rhs =
                        Expr::ltl_weak_until(Expr::complement(p1_rhs), Expr::complement(p2_rhs));
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                11 => {
                    // !(e1 W e2) = !e1 M !e2
                    let new_lhs = Expr::complement(Expr::ltl_weak_until(p1_lhs, p2_lhs));
                    let new_rhs = Expr::ltl_strong_release(
                        Expr::complement(p1_rhs),
                        Expr::complement(p2_rhs),
                    );
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                12 => {
                    // F (e1 + e2) = F e1 + F e2
                    let new_lhs = Expr::ltl_finally(Expr::union(p1_lhs, p2_lhs));
                    let new_rhs = Expr::union(Expr::ltl_finally(p1_rhs), Expr::ltl_finally(p2_rhs));
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                13 => {
                    // G (e1 & e2) = G e1 & G e2
                    let new_lhs = Expr::ltl_globally(Expr::intersect(p1_lhs, p2_lhs));
                    let new_rhs =
                        Expr::intersect(Expr::ltl_globally(p1_rhs), Expr::ltl_globally(p2_rhs));
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                14 => {
                    // X (e1 U e2) = (X e1) U (X e2)
                    let new_lhs = Expr::ltl_next(Expr::ltl_until(p1_lhs, p2_lhs));
                    let new_rhs = Expr::ltl_until(Expr::ltl_next(p1_rhs), Expr::ltl_next(p2_rhs));
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                15 => {
                    // G (F e1 \/ F e2) = G (F e1) \/ G (F e2)
                    let new_lhs = Expr::ltl_globally(Expr::union(
                        Expr::ltl_finally(p1_lhs),
                        Expr::ltl_finally(p2_lhs),
                    ));
                    let new_rhs = Expr::union(
                        Expr::ltl_globally(Expr::ltl_finally(p1_rhs)),
                        Expr::ltl_globally(Expr::ltl_finally(p2_rhs)),
                    );
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                16 => {
                    // e1 U (e1 U e2) = e1 U e2
                    let new_lhs = Expr::ltl_until(p1_lhs.clone(), Expr::ltl_until(p1_lhs, p2_lhs));
                    let new_rhs = Expr::ltl_until(p1_rhs, p2_rhs);
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                17 => {
                    // (e1 U e2) U e2 = e1 U e2
                    let new_lhs = Expr::ltl_until(Expr::ltl_until(p1_lhs, p2_lhs.clone()), p2_lhs);
                    let new_rhs = Expr::ltl_until(p1_rhs, p2_rhs);
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                _ => unreachable!(),
            }
        }
        3 => {
            let (p1_lhs, p1_rhs) = genax(ax_depth - 1, expr_depth, num_fields);
            let (p2_lhs, p2_rhs) = genax(ax_depth - 1, expr_depth, num_fields);
            let (p3_lhs, p3_rhs) = genax(ax_depth - 1, expr_depth, num_fields);
            match rand::random_range(0..7) {
                0 => {
                    // KA-PLUS-ASSOC: p + (q + r) = (p + q) + r
                    let new_lhs = Expr::union(p1_lhs, Expr::union(p2_lhs, p3_lhs));
                    let new_rhs = Expr::union(Expr::union(p1_rhs, p2_rhs), p3_rhs);
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                1 => {
                    // KA-SEQ-ASSOC: p . (q . r) = (p . q) . r
                    let new_lhs = Expr::sequence(p1_lhs, Expr::sequence(p2_lhs, p3_lhs));
                    let new_rhs = Expr::sequence(Expr::sequence(p1_rhs, p2_rhs), p3_rhs);
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                2 => {
                    // KA-SEQ-DIST-L: p . (q + r) = p . q + p . r
                    let new_lhs = Expr::sequence(p1_lhs, Expr::union(p2_lhs, p3_lhs));
                    let new_rhs = Expr::union(
                        Expr::sequence(p1_rhs.clone(), p2_rhs),
                        Expr::sequence(p1_rhs, p3_rhs),
                    );
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                3 => {
                    // KA-SEQ-DIST-R: (p + q) . r = p . r + q . r
                    let new_lhs = Expr::sequence(Expr::union(p1_lhs, p2_lhs), p3_lhs);
                    let new_rhs = Expr::union(
                        Expr::sequence(p1_rhs, p3_rhs.clone()),
                        Expr::sequence(p2_rhs, p3_rhs),
                    );
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                4 => {
                    // BA-PLUS-DIST: a + (b & c) = (a + b) & (a + c)
                    let new_lhs = Expr::union(p1_lhs, Expr::intersect(p2_lhs, p3_lhs));
                    let new_rhs = Expr::intersect(
                        Expr::union(p1_rhs.clone(), p2_rhs),
                        Expr::union(p1_rhs, p3_rhs),
                    );
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                5 => {
                    // e1 U (e2 + e3) = (e1 U e2) + (e1 U e3)
                    let new_lhs = Expr::ltl_until(p1_lhs, Expr::union(p2_lhs, p3_lhs));
                    let new_rhs = Expr::union(
                        Expr::ltl_until(p1_rhs.clone(), p2_rhs),
                        Expr::ltl_until(p1_rhs, p3_rhs),
                    );
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                6 => {
                    // (e1 & e2) U e3 = (e1 U e3) & (e2 U e3)
                    let new_lhs = Expr::ltl_until(Expr::intersect(p1_lhs, p2_lhs), p3_lhs);
                    let new_rhs = Expr::intersect(
                        Expr::ltl_until(p1_rhs, p3_rhs.clone()),
                        Expr::ltl_until(p2_rhs, p3_rhs),
                    );
                    return flip_equality_rand(new_lhs, new_rhs);
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}

/// Generates a pair of expressions where e1 <= e2 (i.e., e1 + e2 = e2)
///
/// - `n` (`ax_depth`): Controls the depth of recursion.
/// - `d` (`expr_depth`): Controls the depth of the generated expression
/// - `k` (`num_fields`): Controls the maximum number of distinct variables (fields `x0` to `xk-1`).
pub fn gen_leq(ax_depth: usize, expr_depth: usize, num_fields: u32) -> (Exp, Exp) {
    assert!(
        num_fields >= 2,
        "num_fields must be >= 2 to generate distinct fields"
    );

    if ax_depth == 0 {
        // Base case: generate two random expressions where one is <= the other
        let e = gen_random_expr(num_fields, expr_depth);
        let random_expr = gen_random_expr(num_fields, expr_depth / 2);

        // e <= e + random_expr (by definition of <=)
        return (e.clone(), Expr::union(e, random_expr));
    }

    // Recursive step: choose a method to generate e1 <= e2
    match rand::random_range(0..4) {
        0 => {
            // Method 1: Use genax to get equal expressions, then add something to rhs
            let (e1, e2) = genax(ax_depth - 1, expr_depth, num_fields);
            let random_expr = gen_random_expr(num_fields, expr_depth / 2);

            // If e1 = e2, then e1 <= e2 + random_expr
            (e1, Expr::union(e2, random_expr))
        }
        1 => {
            // Method 2: Use gen_leq recursively and add something to rhs
            let (e1, e2) = gen_leq(ax_depth - 1, expr_depth, num_fields);
            let random_expr = gen_random_expr(num_fields, expr_depth / 2);

            // If e1 <= e2, then e1 <= e2 + random_expr
            (e1, Expr::union(e2, random_expr))
        }
        2 => {
            // Method 3: Strong operators <= Weak operators
            let e1 = gen_random_expr(num_fields, expr_depth);
            let e2 = gen_random_expr(num_fields, expr_depth);

            match rand::random_range(0..3) {
                0 => {
                    // Strong until <= Weak until
                    // e1 U e2 <= e1 W e2
                    (
                        Expr::ltl_until(e1.clone(), e2.clone()),
                        Expr::ltl_weak_until(e1, e2),
                    )
                }
                1 => {
                    // Strong release >= Weak release (M is stronger than R)
                    // e1 R e2 <= e1 M e2  (weak release is weaker than strong release)
                    (
                        Expr::ltl_strong_release(e1.clone(), e2.clone()),
                        Expr::ltl_release(e1, e2),
                    )
                }
                2 => {
                    // Strong next <= Weak next
                    // X' e <= X e
                    (Expr::ltl_next(e1.clone()), Expr::ltl_weak_next(e1))
                }
                _ => unreachable!(),
            }
        }
        3 => {
            // Method 4: Combine recursive gen_leq results
            let (e1, e2) = gen_leq(ax_depth - 1, expr_depth, num_fields);
            let (e3, e4) = gen_leq(ax_depth - 1, expr_depth, num_fields);

            match rand::random_range(0..3) {
                0 => {
                    // If e1 <= e2 and e3 <= e4, then e1 + e3 <= e2 + e4
                    (Expr::union(e1, e3), Expr::union(e2, e4))
                }
                1 => {
                    // If e1 <= e2 and e3 <= e4, then e1 & e3 <= e2 & e4
                    (Expr::intersect(e1, e3), Expr::intersect(e2, e4))
                }
                2 => {
                    // If e1 <= e2 and e3 <= e4, then e1;e3 <= e2;e4
                    (Expr::sequence(e1, e3), Expr::sequence(e2, e4))
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
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

    /// Generates pairs of non-equivalent terms and prints them to stdout
    #[test]
    fn print_random_non_equivalent_terms() {
        let ax_depth = 2;
        let expr_depth = 1;
        let num_fields = 3;

        let max_trials = 5;
        for n in 0..=ax_depth {
            for _ in 0..=max_trials {
                let e1 = gen_random_expr(num_fields, expr_depth);
                let e2 = gen_random_expr(num_fields, expr_depth);
                let xor = Expr::xor(e1.clone(), e2.clone());
                let mut aut = Aut::new(num_fields);
                let state = aut.expr_to_state(&xor);
                if !aut.is_empty(state) {
                    println!("{:?} != {:?}", e1, e2);
                }
            }
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

        // Max no. of trials to run
        let max_trials = 10000;
        let mut num_trials = 0;

        // For each `n`, test whether the emptiness check
        // passes for `max_trials` rounds
        for n in 0..=ax_depth {
            while num_trials <= max_trials {
                println!("ax_depth n = {}\n", n);
                let (e1, e2) = genax(n, expr_depth, num_fields);
                println!("Checking xor of\n  {}\n   ===\n  {}\n", e1, e2);
                let xor = Expr::xor(e1.clone(), e2.clone());
                println!("XOR result = {}\n", xor);
                let mut aut = Aut::new(num_fields);
                let state = aut.expr_to_state(&xor);
                if aut.is_empty(state) {
                    println!("Success!\n");
                    num_trials += 1;
                } else {
                    assert!(false, "Failure!\n");
                }
            }
        }
    }

    #[test]
    fn fuzz_test_leq() {
        // Enable backtrace for debugging failing tests
        unsafe {
            std::env::set_var("RUST_BACKTRACE", "1");
        }

        // Generate random expressions with e1 <= e2, and check if e1 + e2 = e2
        let ax_depth = 3;
        let expr_depth = 1;
        let num_fields = 3;

        // Max no. of trials to run
        let max_trials = 10000;
        let mut num_trials = 0;

        // For each `n`, test whether the emptiness check
        // passes for `max_trials` rounds
        for n in 0..=ax_depth {
            while num_trials <= max_trials {
                println!("ax_depth n = {}\n", n);
                let (e1, e2) = gen_leq(n, expr_depth, num_fields);
                println!("Checking e1 <= e2 for\n  {}\n   <=\n  {}\n", e1, e2);

                // e1 <= e2 means e1 + e2 = e2
                // So we need to check if (e1 + e2) xor e2 = 0
                let e1_plus_e2 = Expr::union(e1.clone(), e2.clone());
                let xor = Expr::xor(e1_plus_e2, e2.clone());

                println!("XOR result = {}\n", xor);
                let mut aut = Aut::new(num_fields);
                let state = aut.expr_to_state(&xor);
                if aut.is_empty(state) {
                    println!("Success!\n");
                    num_trials += 1;
                } else {
                    assert!(
                        false,
                        "Failure for e1 <= e2!\n  e1 = {}\n  e2 = {}\n",
                        e1, e2
                    );
                }
            }
        }
    }

    /// Regression test: confirms that the emptiness check fails for two terms
    /// that are not equivalent (taken from a corpus of known inequivalent terms)
    #[test]
    fn test_neq() {
        let num_fields = 3;
        let non_equivalent_terms = vec![
            (Expr::complement(Expr::one()), Expr::ltl_next(Expr::top())),
            (
                Expr::ltl_until(Expr::dup(), Expr::dup()),
                Expr::complement(Expr::ltl_until(Expr::top(), Expr::complement(Expr::dup()))),
            ),
            (
                Expr::ltl_until(Expr::top(), Expr::top()),
                Expr::sequence(Expr::test(2, false), Expr::top()),
            ),
            (
                Expr::star(Expr::zero()),
                Expr::complement(Expr::assign(1, false)),
            ),
            (
                Expr::union(
                    Expr::ltl_until(Expr::dup(), Expr::dup()),
                    Expr::complement(Expr::ltl_until(Expr::top(), Expr::complement(Expr::dup()))),
                ),
                Expr::sequence(Expr::top(), Expr::test(0, true)),
            ),
            (
                Expr::star(Expr::one()),
                Expr::complement(Expr::ltl_until(Expr::top(), Expr::complement(Expr::zero()))),
            ),
            (
                Expr::intersect(
                    Expr::complement(Expr::ltl_until(
                        Expr::complement(Expr::end()),
                        Expr::complement(Expr::zero()),
                    )),
                    Expr::ltl_until(Expr::top(), Expr::end()),
                ),
                Expr::xor(Expr::top(), Expr::zero()),
            ),
            (
                Expr::complement(Expr::ltl_until(
                    Expr::complement(Expr::zero()),
                    Expr::complement(Expr::assign(1, false)),
                )),
                Expr::union(Expr::test(0, false), Expr::one()),
            ),
            (
                Expr::ltl_until(Expr::zero(), Expr::top()),
                Expr::intersect(
                    Expr::complement(Expr::ltl_until(
                        Expr::complement(Expr::one()),
                        Expr::complement(Expr::test(1, false)),
                    )),
                    Expr::ltl_until(Expr::top(), Expr::one()),
                ),
            ),
            (
                Expr::union(Expr::ltl_next(Expr::zero()), Expr::end()),
                Expr::xor(Expr::one(), Expr::test(1, true)),
            ),
            (
                Expr::sequence(Expr::top(), Expr::assign(1, true)),
                Expr::ltl_until(Expr::top(), Expr::assign(2, false)),
            ),
            (
                Expr::union(Expr::test(2, false), Expr::one()),
                Expr::ltl_until(Expr::zero(), Expr::zero()),
            ),
            (Expr::top(), Expr::complement(Expr::assign(0, false))),
        ];

        // For each `n`, test whether the emptiness check
        // fails for `max_trials` rounds
        for (e1, e2) in non_equivalent_terms {
            let xor = Expr::xor(e1.clone(), e2.clone());
            println!("XOR result = {}\n", xor);
            let mut aut = Aut::new(num_fields);
            let state = aut.expr_to_state(&xor);
            if !aut.is_empty(state) {
                println!("Success!\n");
            } else {
                assert!(
                    false,
                    "Failure! Expected XOR of {} and {} to be non-empty \n",
                    e1, e2
                );
            }
        }
    }
}
