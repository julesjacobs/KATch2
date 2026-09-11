use katch2::{sp::SP, spp::{SPP, SPPstore}};

fn packet(m: &mut SPPstore, n: u32, value: usize) -> SP {
    let mut result = m.sp.one;
    for bit in 0..n {
        let test = m.sp.test(bit, value & (1 << bit) != 0);
        result = m.sp.intersect(result, test);
    }
    result
}

fn packet_set(m: &mut SPPstore, points: &[SP], mask: u64) -> SP {
    let mut result = m.sp.zero;
    for (i, &point) in points.iter().enumerate() {
        if mask & (1 << i) != 0 { result = m.sp.union(result, point); }
    }
    result
}

fn relation(m: &mut SPPstore, atoms: &[SPP], mask: u64) -> SPP {
    let mut result = m.zero;
    for (i, &atom) in atoms.iter().enumerate() {
        if mask & (1 << i) != 0 { result = m.union(result, atom); }
    }
    result
}

fn random(state: &mut u64) -> u64 {
    *state ^= *state << 13;
    *state ^= *state >> 7;
    *state ^= *state << 17;
    *state
}

#[test]
fn packet_images_match_finite_relations() {
    let mut seed = 42;
    for n in 0..=3 {
        let size = 1 << n;
        let mut m = SPPstore::new(n);
        let points: Vec<_> = (0..size).map(|i| packet(&mut m, n, i)).collect();
        let mut atoms = vec![];
        for input in 0..size {
            for output in 0..size {
                let mut atom = m.one;
                for bit in 0..n {
                    let test = m.test(bit, input & (1 << bit) != 0);
                    atom = m.sequence(atom, test);
                }
                for bit in 0..n {
                    let assign = m.assign(bit, output & (1 << bit) != 0);
                    atom = m.sequence(atom, assign);
                }
                atoms.push(atom);
            }
        }
        for _ in 0..64 {
            let mask = random(&mut seed);
            let rel = relation(&mut m, &atoms, mask);
            for _ in 0..16 {
                let set_mask = random(&mut seed);
                let set = packet_set(&mut m, &points, set_mask);
                let mut forward = 0;
                let mut backward = 0;
                for input in 0..size {
                    for output in 0..size {
                        if mask & (1 << (input * size + output)) != 0 {
                            if set_mask & (1 << input) != 0 { forward |= 1 << output; }
                            if set_mask & (1 << output) != 0 { backward |= 1 << input; }
                        }
                    }
                }
                let expected_forward = packet_set(&mut m, &points, forward);
                let expected_backward = packet_set(&mut m, &points, backward);
                assert_eq!(m.has_image(set, rel), forward != 0, "depth={n}");
                assert_eq!(m.push(set, rel), expected_forward, "depth={n}");
                assert_eq!(m.pull(rel, set), expected_backward, "depth={n}");
            }
        }
    }
}

#[test]
fn relation_operations_match_finite_matrices() {
    let mut seed = 1234567;
    for n in 0..=3 {
        let size = 1 << n;
        let universe = u64::MAX >> (64 - size * size);
        let mut m = SPPstore::new(n);
        let mut atoms = vec![];
        for input in 0..size {
            for output in 0..size {
                let mut atom = m.one;
                for bit in 0..n {
                    let t = m.test(bit, input & (1 << bit) != 0);
                    atom = m.sequence(atom, t);
                }
                for bit in 0..n {
                    let a = m.assign(bit, output & (1 << bit) != 0);
                    atom = m.sequence(atom, a);
                }
                atoms.push(atom);
            }
        }
        for _ in 0..64 {
            let a = random(&mut seed) & universe;
            let b = random(&mut seed) & universe;
            let x = relation(&mut m, &atoms, a);
            let y = relation(&mut m, &atoms, b);
            let expected = relation(&mut m, &atoms, a | b);
            assert_eq!(m.union(x, y), expected);
            let expected = relation(&mut m, &atoms, a & b);
            assert_eq!(m.intersect(x, y), expected);
            let expected = relation(&mut m, &atoms, a ^ b);
            assert_eq!(m.xor(x, y), expected);
            let expected = relation(&mut m, &atoms, a & !b);
            assert_eq!(m.difference(x, y), expected);
            let expected = relation(&mut m, &atoms, !a & universe);
            assert_eq!(m.complement(x), expected);
            let mut composition = 0;
            let mut closure = a;
            for i in 0..size {
                closure |= 1 << (i * size + i);
                for j in 0..size {
                    for k in 0..size {
                        if a & (1 << (i * size + k)) != 0 && b & (1 << (k * size + j)) != 0 {
                            composition |= 1 << (i * size + j);
                        }
                    }
                }
            }
            for k in 0..size {
                for i in 0..size {
                    for j in 0..size {
                        if closure & (1 << (i * size + k)) != 0 && closure & (1 << (k * size + j)) != 0 {
                            closure |= 1 << (i * size + j);
                        }
                    }
                }
            }
            let expected = relation(&mut m, &atoms, composition);
            assert_eq!(m.sequence(x, y), expected);
            let expected = relation(&mut m, &atoms, closure);
            assert_eq!(m.star(x), expected);
        }
    }
}

#[test]
#[should_panic]
fn test_rejects_out_of_range_field() {
    SPPstore::new(2).test(2, false);
}

#[test]
#[should_panic]
fn assignment_rejects_out_of_range_field() {
    SPPstore::new(0).assign(0, true);
}

#[test]
fn constants_and_diagonal_support_deeper_nodes() {
    let mut m = SPPstore::new(1);
    let identity = m.mk(m.one, m.zero, m.zero, m.one);
    let empty = m.xor(identity, identity);
    assert!(m.is_zero(empty));
    assert_eq!(m.get(empty).x00, m.zero);
    assert_eq!(m.star(identity), identity);
    let deeper = m.mk(identity, empty, empty, identity);
    let deeper_empty = m.difference(deeper, deeper);
    assert_eq!(m.get(deeper_empty).x00, empty);
    assert_eq!(m.sequence(deeper, deeper), deeper);
    let universal = m.sp.mk(m.sp.one, m.sp.one);
    assert_eq!(m.diagonal(universal), identity);
    let zero = m.sp.xor(universal, universal);
    assert_eq!(m.sp.get(zero).x0, m.sp.zero);
}

#[test]
fn native_predicate_operations_match_finite_sets() {
    use katch2::expr::Expr;
    let mut m = SPPstore::new(2);
    let points: Vec<_> = (0..4).map(|i| packet(&mut m, 2, i)).collect();
    for a in 0..16 {
        let x = packet_set(&mut m, &points, a);
        let diagonal = m.diagonal(x);
        assert_eq!(m.push(m.sp.one, diagonal), x);
        for &point in &points {
            let expected = m.sp.intersect(point, x);
            assert_eq!(m.push(point, diagonal), expected);
        }
        for b in 0..16 {
            let y = packet_set(&mut m, &points, b);
            let expected = packet_set(&mut m, &points, a ^ b);
            assert_eq!(m.sp.xor(x, y), expected);
            let expected = packet_set(&mut m, &points, a & !b);
            assert_eq!(m.sp.difference(x, y), expected);
        }
    }
    let test = Expr::test_negation(Expr::xor(Expr::test(0, true), Expr::test(1, true)));
    let compiled = m.sp.compile_predicate(&test).unwrap();
    let expected = packet_set(&mut m, &points, 0b1001);
    assert_eq!(compiled, expected);
    assert!(m.sp.compile_predicate(&Expr::complement(Expr::zero())).is_none());
}

#[test]
fn primitive_tests_match_full_depth_builder() {
    use katch2::sp::SPstore;
    for width in [1, 2, 8, 32, 64, 128] {
        let mut m = SPstore::new(width);
        for var in (0..width).rev() {
            for value in [false, true] {
                let actual = m.test(var, value);
                let mut expected = SP::new(1);
                let mut zero = SP::new(0);
                for bit in (0..width).rev() {
                    expected = if bit == var {
                        if value { m.mk(zero, expected) } else { m.mk(expected, zero) }
                    } else { m.mk(expected, expected) };
                    zero = m.mk(zero, zero);
                }
                assert_eq!(actual, expected);
                assert_eq!(m.test(var, value), expected);
            }
        }
    }
}

#[test]
fn relation_projection_wrappers_preserve_operand_depth() {
    let mut m = SPPstore::new(1);
    let identity = m.mk(m.one, m.zero, m.zero, m.one);
    let universal = m.mk(m.top, m.top, m.top, m.top);
    assert_eq!(m.backward(identity), universal);
    assert_eq!(m.naive_forward(identity), universal);
}
