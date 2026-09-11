//! Run the shared operation-DAG benchmark through the production query API.
//! Usage: query_benchmark FILE.ops REPETITIONS [--stats]
use katch2::aut::{Aut, QueryBuilder};
use std::{env, fs, time::Instant};

fn main() {
    let args: Vec<_> = env::args().collect();
    let text = fs::read_to_string(&args[1]).unwrap();
    let mut lines = text.lines();
    let header: Vec<u32> = lines
        .next()
        .unwrap()
        .split_whitespace()
        .map(|x| x.parse().unwrap())
        .collect();
    let widths = &header[1..];
    assert_eq!(header[0] as usize, widths.len());
    let mut offsets = Vec::new();
    let mut bits: u32 = 0;
    for &width in widths {
        assert!(width < usize::BITS);
        offsets.push(bits);
        bits = bits.checked_add(width).unwrap();
    }
    let ops: Vec<(char, usize, usize)> = lines
        .map(|line| {
            let mut words = line.split_whitespace();
            (
                words.next().unwrap().chars().next().unwrap(),
                words.next().unwrap_or("0").parse().unwrap(),
                words.next().unwrap_or("0").parse().unwrap(),
            )
        })
        .collect();
    for _ in 0..args[2].parse::<usize>().unwrap() {
        let start = Instant::now();
        let (checks, positives, stats, construction, preparation, execution);
        {
            let mut builder = QueryBuilder::with_capacity(ops.len());
            let mut nodes = Vec::new();
            let mut roots = Vec::new();
            let mut expected = Vec::new();
            for (i, &(op, a, b)) in ops.iter().enumerate() {
                let node = match op {
                    '0' => builder.zero(),
                    '1' => builder.one(),
                    't' | 'a' => {
                        assert!(a < widths.len() && b < (1usize << widths[a]));
                        let mut node = builder.one();
                        for j in 0..widths[a] {
                            let bit = (b >> j) & 1 != 0;
                            let atom = if op == 't' {
                                builder.test(offsets[a] + j, bit)
                            } else {
                                builder.assign(offsets[a] + j, bit)
                            };
                            node = builder.sequence(node, atom);
                        }
                        node
                    }
                    'u' | 's' => {
                        assert!(a < i && b < i);
                        if op == 'u' {
                            builder.union(nodes[a], nodes[b])
                        } else {
                            builder.sequence(nodes[a], nodes[b])
                        }
                    }
                    'r' => {
                        assert!(a < i);
                        builder.star(nodes[a])
                    }
                    'c' => {
                        assert!(a < i && b <= 1);
                        roots.push(nodes[a]);
                        expected.push(b != 0);
                        builder.zero()
                    }
                    _ => panic!("invalid operation {op}"),
                };
                nodes.push(node);
            }
            let program = builder.finish(&roots);
            construction = start.elapsed();
            let prepare_start = Instant::now();
            let mut aut = Aut::new(bits);
            let mut query = program.prepare(&mut aut).unwrap();
            preparation = prepare_start.elapsed();
            let execute_start = Instant::now();
            let mut positive = 0;
            for (index, &answer) in expected.iter().enumerate() {
                let result = !query.is_empty(index).unwrap();
                assert_eq!(result, answer, "query {index}");
                positive += usize::from(result);
            }
            execution = execute_start.elapsed();
            checks = expected.len();
            positives = positive;
            stats = query.stats();
        }
        let elapsed = start.elapsed().as_secs_f64() * 1000.;
        println!("{elapsed},{checks},{positives}");
        if args.iter().any(|x| x == "--stats") {
            eprintln!(
                "{stats:?}; construction={construction:?}; preparation={preparation:?}; execution={execution:?}"
            );
        }
    }
}
