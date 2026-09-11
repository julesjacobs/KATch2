use katch2::{aut::Aut, desugar::desugar, expr::Expr};

#[test]
fn negated_xor_preserves_filters_and_histories() {
    let predicates = [Expr::zero(), Expr::one(), Expr::test(0, false), Expr::test(0, true)];
    for a in &predicates {
        for b in &predicates {
            let compact = desugar(&Expr::test_negation(Expr::xor(a.clone(), b.clone()))).unwrap();
            let expanded = desugar(&Expr::union(
                Expr::intersect(Expr::test_negation(a.clone()), Expr::test_negation(b.clone())),
                Expr::intersect(a.clone(), b.clone()),
            )).unwrap();
            let mut aut = Aut::new(1);
            let difference = Expr::xor(
                Expr::sequence(compact, Expr::dup()),
                Expr::sequence(expanded, Expr::dup()),
            );
            let state = aut.expr_to_state(&difference);
            assert!(aut.is_empty(state));
        }
    }
}

#[test]
fn nested_negated_xor_has_linear_size() {
    fn size(e: &Expr) -> usize {
        match e {
            Expr::Xor(a, b) | Expr::Difference(a, b) => 1 + size(a) + size(b),
            Expr::One | Expr::Test(_, _) => 1,
            other => panic!("unexpected expression: {other:?}"),
        }
    }
    let mut e = Expr::test(0, true);
    for _ in 0..20 {
        e = Expr::test_negation(Expr::xor(e, Expr::test(0, false)));
    }
    let result = desugar(&e).unwrap();
    assert!(size(&result) <= 81);
}

#[test]
fn optimization_does_not_hide_invalid_test_negation() {
    for e in [Expr::star(Expr::zero()), Expr::assign(0, false), Expr::dup()] {
        assert!(desugar(&Expr::test_negation(Expr::sequence(Expr::zero(), e))).is_err());
    }
}

#[test]
fn patterns_pad_at_the_high_end_of_little_endian_values() {
    use katch2::expr::Pattern;
    for pattern in [Pattern::Exact(vec![true]), Pattern::IpRange { start: vec![true], end: vec![true, false, false] }] {
        let e = desugar(&Expr::bit_range_match(0, 3, pattern)).unwrap();
        let expected = Expr::intersect(Expr::test(0, true), Expr::intersect(Expr::test(1, false), Expr::test(2, false)));
        let mut aut = Aut::new(3);
        let state = aut.expr_to_state(&Expr::xor(e, expected));
        assert!(aut.is_empty(state));
    }
}

#[test]
fn pattern_range_boundaries_are_checked() {
    use katch2::expr::Pattern;
    let full = Pattern::IpRange { start: vec![false; 128], end: vec![true; 128] };
    assert_eq!(desugar(&Expr::bit_range_match(0, 128, full)).unwrap(), Expr::one());
    assert!(desugar(&Expr::bit_range_match(3, 2, Pattern::Exact(vec![]))).is_err());
    assert!(desugar(&Expr::bit_range_test(3, 2, vec![])).is_err());
    assert!(desugar(&Expr::bit_range_assign(3, 2, vec![])).is_err());
    let reversed = Pattern::IpRange { start: vec![true], end: vec![false] };
    assert!(desugar(&Expr::bit_range_match(0, 3, reversed)).is_err());
}

#[test]
fn alias_subranges_reject_overflow_and_reversed_bounds() {
    let mut env = katch2::desugar::DesugarEnv::new();
    env.add_alias("last".into(), u32::MAX - 1, u32::MAX);
    assert_eq!(env.compute_subrange("last", 0, 1), Some((u32::MAX - 1, u32::MAX)));
    assert_eq!(env.compute_subrange("last", 0, 3), None);
    assert_eq!(env.compute_subrange("last", 1, 0), None);
}

#[test]
fn field_counts_use_exclusive_range_endpoints() {
    use katch2::expr::Expr;
    assert_eq!(Expr::bit_range_test(0, 8, vec![false; 8]).num_fields(), 8);
    assert_eq!(Expr::bit_range_assign(2, 8, vec![false; 6]).num_fields(), 8);
    assert_eq!(Expr::LetBitRange("x".into(), 0, 8, Expr::one()).num_fields(), 8);
    assert_eq!(Expr::test(7, false).num_fields(), 8);
    assert!(std::panic::catch_unwind(|| Expr::test(u32::MAX, false).num_fields()).is_err());
}
