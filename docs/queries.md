# Demand-directed queries

`Aut::is_empty_expr(&expr)` decides emptiness of a desugared expression. The normal expression analysis API uses this path; it compiles a full automaton only when a nonempty result needs witnesses. `expr_to_state`, automaton visualization, and state-based witness APIs retain their existing behavior.

For a positive query `input; body*; output`, the query engine applies the suffix backward to obtain a target packet set, then explores packets reachable from the input. It stops when it reaches the target or exhausts the frontier. Star-free relation fragments use the existing SPP operations. Small step compositions can be combined; larger steps remain factored. For a small step used by many query roots, the planner can instead materialize `body*` once.

Endpoint projection preserves union, sequence and star, but does not generally preserve intersection, difference, XOR or complement of history languages. These operations and temporal operators compile exactly before endpoint projection. Thus `dup` can act as endpoint identity in positive contexts without erasing distinctions under history-sensitive operators. Predicate and star-free relation operations use their exact SP/SPP representations directly.

## Shared programs

Use `QueryBuilder` to preserve sharing across multiple queries. `QueryBuilder::with_capacity` accepts a node-count allocation hint when the batch size is known. Node handles belong to one builder; using a foreign handle panics. `finish` consumes the builder, and the resulting immutable `QueryProgram` can be cloned and prepared independently in multiple engines. Roots are numbered in the order passed to `finish`.

```rust
use katch2::aut::{Aut, QueryBuilder};

let mut b = QueryBuilder::new();
let input = b.test(0, false);
let step = b.assign(0, true);
let reach = b.star(step);
let output = b.test(0, true);
let prefix = b.sequence(input, reach);
let check = b.sequence(prefix, output);
let program = b.finish(&[check, reach, input, output]);

let mut engine = Aut::new(1);
let mut query = program.prepare(&mut engine)?;
assert!(!query.is_empty(0)?);
assert!(query.post(1, 2)?.contains(&[true]));
assert!(query.pre(1, 3)?.contains(&[false]));
# Ok::<(), katch2::aut::QueryError>(())
```

`post` and `pre` compute complete packet sets; their second root must be a syntactic predicate (zero, one, tests, or binary combinations of predicates). Returned sets borrow the engine and expose membership without exposing store-local handles. Packets must have exactly `num_fields()` bits.

`QueryProgram::from_expr` snapshots an existing desugared `Expr`. Later changes to that expression cannot affect the program. Preparation validates every field in the constructed program, including unused nodes, before executing queries. Invalid fields, unsupported frontend syntax and invalid root indices return `QueryError`.

## Reuse and limits

Preparation accepts `QueryOptions`. By default, up to 64 star views are retained, keyed by the star, seed packet set and direction. Positive answers preserve unfinished frontiers for subsequent targets. Eviction or `clear_views()` affects performance only. Set `cached_views` to zero to disable star-view reuse.

`max_star_expansions` bounds demand-directed star expansions per call, including nested stars. Exhaustion returns `QueryError::ExpansionLimit`; a later call can resume with a larger limit through `set_expansion_limit`. Only completed expansions are published to the cache. **This is not a total time or memory limit:** preparation, exact fallback and individual symbolic operations are outside this bound. The underlying engine stores also retain interned diagrams and computed results.

Planning retains DAG sharing, and repeated shared fragments memoize complete images within an evaluation. These temporary image results are discarded after the evaluation.

The planner balances associative unions while preserving shared subexpressions. Prepared endpoint demands and fused input/relation/output checks avoid constructing packet sets solely to decide existence.

For rectangular batches of factorizable inputs, predicates on fields preserved by the star can move to the suffix. Searches then share an input across those invariant values. The planner verifies factorization using canonical predicates, requires full coverage of the factor combinations, and requires enough view-cache slots for the shared inputs. Correlated input predicates and sparse batches retain their original demands.

Plan-selection heuristics use syntax and diagram sizes, not workload names. For multi-root programs, step composition considers at most two factors with at most 256 distinct relation nodes in total. A star with a relation body of at most 256 nodes, at least 64 query roots and at least 16 distinct inputs can be materialized during preparation. This materialization is disabled when an expansion budget is supplied. The node threshold measures the step, not the resulting closure; it is a cost heuristic rather than a total resource bound.

`stats()` reports compilation visits, image evaluations, star expansions, reused views, shared input groups, composed steps, history barriers and canonical star compilation. Positive queries need no history barriers. Canonical stars can result from either exact-history fallback or selection of a materialized closure for a small batch step.

## Reproducing routing measurements

`cargo build --release --example query_benchmark` builds a driver using this public API. Run `query_benchmark FILE.ops REPETITIONS [--stats]`. Its CSV output is elapsed milliseconds, check count and positive count. Every expected answer is checked. Timings include DAG construction, preparation, all query checks and destruction; file parsing is excluded. `--stats` also reports preparation and execution separately.
