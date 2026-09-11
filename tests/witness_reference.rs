use katch2::{aut::Aut, expr::Expr};

#[test]
fn pruning_removes_dead_target_packets() {
    let mut aut = Aut::new(1);
    let expression = Expr::union(Expr::one(), Expr::sequence(Expr::dup(), Expr::test(0, false)));
    let state = aut.expr_to_state(&expression);
    let transitions = aut.delta_pruned(state);
    assert!(!transitions.get_transitions().is_empty());
    let m = aut.spp_store_mut();
    let input = m.sp.test(0, true);
    for &label in transitions.get_transitions().values() {
        assert_eq!(m.push(input, label), m.sp.zero);
    }
    for _ in 0..100 {
        let (trace, output) = aut.random_trace(state, 3).unwrap();
        assert!(output.is_some());
        if trace[0][0] { assert_eq!(trace.len(), 1); }
    }
}

#[test]
fn cached_derivatives_do_not_exhaust_a_lifetime_budget() {
    let mut aut = Aut::new(0);
    let state = aut.expr_to_state(&Expr::dup());
    let expected = aut.delta(state);
    for _ in 0..100_002 { assert_eq!(aut.delta(state), expected); }
}
