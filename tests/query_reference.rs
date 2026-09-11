use katch2::{
    aut::{Aut, QueryBuilder, QueryError, QueryOptions, QueryProgram},
    expr::Expr,
};

fn point(b: &mut QueryBuilder, bits: u32, value: usize) -> katch2::aut::QueryNode {
    let mut p = b.one();
    for bit in 0..bits {
        let t = b.test(bit, value & (1 << bit) != 0);
        p = b.sequence(p, t);
    }
    p
}
fn edge(b: &mut QueryBuilder, bits: u32, input: usize, output: usize) -> katch2::aut::QueryNode {
    let mut r = point(b, bits, input);
    for bit in 0..bits {
        let a = b.assign(bit, output & (1 << bit) != 0);
        r = b.sequence(r, a);
    }
    r
}
fn compare(expr: &Expr, bits: u32) {
    let mut exact = Aut::new(bits);
    let state = exact.expr_to_state(expr);
    let expected = exact.is_empty(state);
    let mut queried = Aut::new(bits);
    assert_eq!(queried.is_empty_expr(expr).unwrap(), expected, "{expr}");
}

#[test]
fn exhaustive_one_bit_stars_images_and_preimages() {
    for mask in 0..16 {
        let mut b = QueryBuilder::new();
        let mut relation = b.zero();
        for i in 0..2 {
            for j in 0..2 {
                if mask & (1 << (2 * i + j)) != 0 {
                    let e = edge(&mut b, 1, i, j);
                    relation = b.union(relation, e);
                }
            }
        }
        let star = b.star(relation);
        let sets = [b.zero(), b.test(0, false), b.test(0, true), b.one()];
        let mut roots = vec![star];
        roots.extend(sets);
        for &input in &sets {
            for &output in &sets {
                let prefix = b.sequence(input, star);
                roots.push(b.sequence(prefix, output));
            }
        }
        let program = b.finish(&roots);
        let mut aut = Aut::new(1);
        let mut query = program.prepare(&mut aut).unwrap();
        let closure = mask | 0b1001;
        for input in 0..4 {
            for output in 0..4 {
                let expected = (0..2).any(|i| {
                    (0..2).any(|j| {
                        input & (1 << i) != 0
                            && output & (1 << j) != 0
                            && closure & (1 << (2 * i + j)) != 0
                    })
                });
                assert_eq!(!query.is_empty(5 + 4 * input + output).unwrap(), expected);
            }
            let post = query.post(0, 1 + input).unwrap();
            for j in 0..2 {
                let expected =
                    (0..2).any(|i| input & (1 << i) != 0 && closure & (1 << (2 * i + j)) != 0);
                assert_eq!(post.contains(&[j != 0]), expected);
            }
            let pre = query.pre(0, 1 + input).unwrap();
            for i in 0..2 {
                let expected =
                    (0..2).any(|j| input & (1 << j) != 0 && closure & (1 << (2 * i + j)) != 0);
                assert_eq!(pre.contains(&[i != 0]), expected);
            }
        }
        assert_eq!(query.stats().canonical_stars, 0);
        assert_eq!(query.stats().history_barriers, 0);
    }
}

#[test]
fn resumable_views_survive_positive_answers_eviction_and_limits() {
    let mut b = QueryBuilder::new();
    let e01 = edge(&mut b, 2, 0, 1);
    let e12 = edge(&mut b, 2, 1, 2);
    let body = b.union(e01, e12);
    let star = b.star(body);
    let input = point(&mut b, 2, 0);
    let prefix = b.sequence(input, star);
    let goals: Vec<_> = (0..4).map(|i| point(&mut b, 2, i)).collect();
    let mut roots: Vec<_> = goals.iter().map(|&goal| b.sequence(prefix, goal)).collect();
    let from3 = b.sequence(goals[3], star);
    roots.push(b.sequence(from3, goals[0]));
    let program = b.finish(&roots);
    for cached_views in [0, 1, 64] {
        let mut aut = Aut::new(2);
        let mut q = program
            .prepare_with_options(
                &mut aut,
                QueryOptions {
                    cached_views,
                    max_star_expansions: Some(0),
                },
            )
            .unwrap();
        assert!(!q.is_empty(0).unwrap());
        assert_eq!(q.is_empty(2), Err(QueryError::ExpansionLimit(0)));
        q.set_expansion_limit(Some(1));
        assert_eq!(q.is_empty(2), Err(QueryError::ExpansionLimit(1)));
        q.set_expansion_limit(None);
        assert!(!q.is_empty(1).unwrap());
        assert!(!q.is_empty(2).unwrap());
        assert!(q.is_empty(4).unwrap());
        assert!(q.is_empty(3).unwrap());
        q.clear_views();
        assert!(!q.is_empty(2).unwrap());
    }
}

#[test]
fn errors_during_nested_expansion_preserve_outer_progress() {
    let mut b = QueryBuilder::new();
    let e01 = edge(&mut b, 2, 0, 1);
    let e12 = edge(&mut b, 2, 1, 2);
    let inner = b.star(e01);
    let body = b.sequence(inner, e12);
    let outer = b.star(body);
    let zero = point(&mut b, 2, 0);
    let two = point(&mut b, 2, 2);
    let prefix = b.sequence(zero, outer);
    let root = b.sequence(prefix, two);
    let program = b.finish(&[root]);
    for limit in 0..5 {
        let mut aut = Aut::new(2);
        let mut q = program
            .prepare_with_options(
                &mut aut,
                QueryOptions {
                    cached_views: 2,
                    max_star_expansions: Some(limit),
                },
            )
            .unwrap();
        let first = q.is_empty(0);
        assert!(first.is_err() || first == Ok(false));
        q.set_expansion_limit(None);
        assert_eq!(q.is_empty(0), Ok(false));
    }
}

#[test]
fn history_boolean_and_temporal_barriers_remain_exact() {
    let e = Expr::sequence(
        Expr::assign(0, false),
        Expr::sequence(Expr::dup(), Expr::assign(0, true)),
    );
    let f = Expr::sequence(Expr::assign(0, true), Expr::dup());
    let cases = [
        Expr::intersect(e.clone(), f.clone()),
        Expr::difference(e.clone(), f.clone()),
        Expr::xor(e, f),
        Expr::difference(Expr::star(Expr::dup()), Expr::one()),
        Expr::intersect(
            Expr::star(Expr::dup()),
            Expr::sequence(Expr::dup(), Expr::dup()),
        ),
        Expr::intersect(Expr::ltl_next(Expr::one()), Box::new(Expr::End)),
        Expr::complement(Expr::star(Expr::dup())),
        Expr::sequence(
            Expr::test(0, false),
            Expr::star(Expr::union(Expr::assign(0, true), Expr::dup())),
        ),
        Expr::ltl_until(Expr::test(0, false), Expr::test(0, true)),
        Expr::intersect(Expr::test(0, false), Expr::assign(0, true)),
        Expr::difference(Expr::assign(0, false), Expr::assign(0, true)),
    ];
    for expr in cases {
        compare(&expr, 1);
    }
}

#[test]
fn target_demand_is_propagated_through_relational_suffixes() {
    for value in [false, true] {
        let expr = Expr::sequence(
            Expr::test(0, false),
            Expr::sequence(
                Expr::star(Expr::assign(0, true)),
                Expr::sequence(Expr::assign(1, value), Expr::test(1, false)),
            ),
        );
        compare(&expr, 2);
        let program = QueryProgram::from_expr(&expr).unwrap();
        let mut aut = Aut::new(2);
        let mut query = program.prepare(&mut aut).unwrap();
        assert_eq!(query.is_empty(0).unwrap(), value);
        assert_eq!(query.stats().canonical_stars, 0);
    }
}

#[test]
fn preparation_validates_before_short_circuiting_and_snapshots_syntax() {
    let invalid = Expr::sequence(Expr::zero(), Expr::test(2, true));
    assert!(matches!(
        Aut::new(2).is_empty_expr(&invalid),
        Err(QueryError::FieldOutOfRange { .. })
    ));
    let raw = Expr::sequence(Expr::zero(), Expr::test_negation(Expr::star(Expr::zero())));
    assert!(matches!(
        QueryProgram::from_expr(&raw),
        Err(QueryError::NotDesugared)
    ));
    let mut expr = Expr::sequence(Expr::test(0, false), Expr::test(0, true));
    let program = QueryProgram::from_expr(&expr).unwrap();
    if let Expr::Sequence(_, right) = expr.as_mut() {
        **right = Expr::One;
    }
    for bits in [1, 2] {
        let mut aut = Aut::new(bits);
        assert!(program.prepare(&mut aut).unwrap().is_empty(0).unwrap());
        assert!(!aut.is_empty_expr(&expr).unwrap());
    }
    let mut a = QueryBuilder::new();
    let x = a.one();
    let mut b = QueryBuilder::new();
    let y = b.one();
    assert!(std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| b.sequence(x, y))).is_err());
}

#[test]
fn deeply_nested_shared_programs_use_iterative_traversal() {
    let mut b = QueryBuilder::new();
    let mut body = b.dup();
    for _ in 0..10000 {
        body = b.star(body);
    }
    let input = b.test(0, false);
    let output = b.test(0, true);
    let prefix = b.sequence(input, body);
    let query = b.sequence(prefix, output);
    let program = b.finish(&[query]);
    let mut aut = Aut::new(1);
    let mut q = program.prepare(&mut aut).unwrap();
    assert!(q.is_empty(0).unwrap());
    assert_eq!(q.stats().canonical_stars, 0);
}

#[test]
fn relation_boolean_operations_preserve_input_correlation() {
    let a = Expr::sequence(Expr::test(0, false), Expr::assign(0, false));
    let b = Expr::sequence(Expr::test(0, true), Expr::assign(0, false));
    for (expr, empty) in [
        (Expr::intersect(a.clone(), b.clone()), true),
        (Expr::difference(a.clone(), b.clone()), false),
        (Expr::xor(a, b), false),
    ] {
        assert_eq!(Aut::new(1).is_empty_expr(&expr).unwrap(), empty);
        compare(&expr, 1);
    }
}

#[test]
fn api_errors_and_zero_iteration_preserve_untouched_fields() {
    let mut b = QueryBuilder::new();
    let step = b.assign(0, true);
    let star = b.star(step);
    let input = point(&mut b, 2, 2);
    let zero = b.zero();
    let program = b.finish(&[star, input, zero]);
    let mut aut = Aut::new(2);
    let mut q = program.prepare(&mut aut).unwrap();
    assert_eq!(q.is_empty(3), Err(QueryError::UnknownRoot(3)));
    assert!(matches!(q.post(0, 3), Err(QueryError::UnknownRoot(3))));
    assert!(matches!(q.pre(0, 0), Err(QueryError::NotPredicateRoot(0))));
    assert!(q.post(0, 2).unwrap().is_empty());
    let set = q.post(0, 1).unwrap();
    assert_eq!(set.num_fields(), 2);
    for value in 0..4 {
        assert_eq!(set.contains(&[value & 1 != 0, value & 2 != 0]), value >= 2);
    }
}

#[test]
fn bounded_deterministic_expressions_match_exact_compilation() {
    fn expr(seed: &mut u64, depth: usize) -> Box<Expr> {
        *seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
        let choice = (*seed >> 32) as usize;
        if depth == 0 {
            return match choice % 7 {
                0 => Expr::zero(),
                1 => Expr::one(),
                2 => Expr::dup(),
                3 => Box::new(Expr::End),
                4 => Expr::test(0, false),
                5 => Expr::assign(0, true),
                _ => Expr::test(0, true),
            };
        }
        let a = expr(seed, depth - 1);
        match choice % 9 {
            0 => Expr::star(a),
            1 => Expr::complement(a),
            2 => Expr::ltl_next(a),
            op => {
                let b = expr(seed, depth - 1);
                match op {
                    3 => Expr::union(a, b),
                    4 => Expr::sequence(a, b),
                    5 => Expr::intersect(a, b),
                    6 => Expr::difference(a, b),
                    7 => Expr::xor(a, b),
                    _ => Expr::ltl_until(a, b),
                }
            }
        }
    }
    let mut seed = 419;
    for _ in 0..128 {
        let body = expr(&mut seed, 2);
        for input in [false, true] {
            for output in [false, true] {
                compare(
                    &Expr::sequence(
                        Expr::test(0, input),
                        Expr::sequence(body.clone(), Expr::test(0, output)),
                    ),
                    1,
                );
            }
        }
    }
}

#[test]
fn sequence_and_union_planning_preserve_dag_sharing() {
    for nonflat in [false, true] {
        let mut b = QueryBuilder::new();
        let assignment = b.assign(0, true);
        let mut body = if nonflat {
            b.star(assignment)
        } else {
            assignment
        };
        for _ in 0..40 {
            body = b.sequence(body, body);
        }
        let input = b.test(0, false);
        let output = b.test(1, true);
        let prefix = b.sequence(input, body);
        let root = b.sequence(prefix, output);
        let program = b.finish(&[root, body, input]);
        let mut aut = Aut::new(2);
        let mut query = program.prepare(&mut aut).unwrap();
        assert!(!query.is_empty(0).unwrap());
        assert!(query.post(1, 2).unwrap().contains(&[true, false]));
        assert!(query.pre(1, 2).unwrap().is_empty() != nonflat);
        assert!(query.stats().image_evaluations < 20000);
        assert_eq!(query.stats().canonical_stars, 0);
    }
    let mut b = QueryBuilder::new();
    let step = b.assign(0, true);
    let mut body = b.star(step);
    for _ in 0..40 {
        body = b.union(body, body);
    }
    let before = point(&mut b, 2, 0);
    let after = point(&mut b, 2, 2);
    let prefix = b.sequence(before, body);
    let root = b.sequence(prefix, after);
    let program = b.finish(&[root]);
    let mut aut = Aut::new(2);
    let mut query = program.prepare(&mut aut).unwrap();
    assert!(query.is_empty(0).unwrap());
    assert!(query.stats().image_evaluations < 1000);
}
