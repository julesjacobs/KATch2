//! Demand-directed endpoint queries over immutable expression DAGs.
//!
//! Positive operators preserve endpoint reachability under composition. Boolean
//! combinations of history languages and temporal operators instead use the
//! exact automaton before projecting endpoints. Canonical compilation is separate.

use super::{Aut, Compiled};
use crate::{expr::Expr, sp::SP, spp::SPP};
use rustc_hash::FxHashMap;
use std::{
    collections::VecDeque,
    fmt,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

static NEXT_BUILDER: AtomicUsize = AtomicUsize::new(1);

/// A node belonging to one QueryBuilder. Passing it to another builder panics.
#[derive(Clone, Copy, Debug)]
pub struct QueryNode {
    owner: usize,
    index: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Binary {
    Union,
    Intersect,
    Xor,
    Difference,
    Sequence,
    Until,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Unary {
    Star,
    Complement,
    Next,
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Node {
    Zero,
    One,
    Top,
    End,
    Dup,
    Test(u32, bool),
    Assign(u32, bool),
    Binary(Binary, usize, usize),
    Unary(Unary, usize),
}
impl Node {
    fn children(self) -> impl Iterator<Item = usize> {
        let pair = match self {
            Self::Binary(_, a, b) => [Some(a), Some(b)],
            Self::Unary(_, a) => [Some(a), None],
            _ => [None, None],
        };
        pair.into_iter().flatten()
    }
}

/// Constructs a shared DAG without expanding repeated subexpressions into trees.
/// All nodes are immutable once inserted; finish consumes the builder.
pub struct QueryBuilder {
    owner: usize,
    nodes: Vec<Node>,
    intern: FxHashMap<Node, usize>,
    max_field: Option<u32>,
}
impl Default for QueryBuilder {
    fn default() -> Self {
        Self::new()
    }
}
impl QueryBuilder {
    pub fn new() -> Self {
        Self {
            owner: NEXT_BUILDER
                .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |n| n.checked_add(1))
                .expect("query builder identifiers exhausted"),
            nodes: Vec::new(),
            intern: FxHashMap::default(),
            max_field: None,
        }
    }
    fn insert(&mut self, node: Node) -> QueryNode {
        if let Node::Test(field, _) | Node::Assign(field, _) = node {
            self.max_field = Some(self.max_field.map_or(field, |old| old.max(field)));
        }
        let index = *self.intern.entry(node).or_insert_with(|| {
            let index = self.nodes.len();
            self.nodes.push(node);
            index
        });
        QueryNode {
            owner: self.owner,
            index,
        }
    }
    fn index(&self, node: QueryNode) -> usize {
        assert_eq!(
            node.owner, self.owner,
            "query node belongs to another builder"
        );
        node.index
    }
    fn binary(&mut self, op: Binary, a: QueryNode, b: QueryNode) -> QueryNode {
        self.insert(Node::Binary(op, self.index(a), self.index(b)))
    }
    fn unary(&mut self, op: Unary, a: QueryNode) -> QueryNode {
        self.insert(Node::Unary(op, self.index(a)))
    }
    pub fn zero(&mut self) -> QueryNode {
        self.insert(Node::Zero)
    }
    pub fn one(&mut self) -> QueryNode {
        self.insert(Node::One)
    }
    pub fn top(&mut self) -> QueryNode {
        self.insert(Node::Top)
    }
    pub fn end(&mut self) -> QueryNode {
        self.insert(Node::End)
    }
    pub fn dup(&mut self) -> QueryNode {
        self.insert(Node::Dup)
    }
    pub fn test(&mut self, field: u32, value: bool) -> QueryNode {
        self.insert(Node::Test(field, value))
    }
    pub fn assign(&mut self, field: u32, value: bool) -> QueryNode {
        self.insert(Node::Assign(field, value))
    }
    pub fn union(&mut self, a: QueryNode, b: QueryNode) -> QueryNode {
        self.binary(Binary::Union, a, b)
    }
    pub fn intersect(&mut self, a: QueryNode, b: QueryNode) -> QueryNode {
        self.binary(Binary::Intersect, a, b)
    }
    pub fn xor(&mut self, a: QueryNode, b: QueryNode) -> QueryNode {
        self.binary(Binary::Xor, a, b)
    }
    pub fn difference(&mut self, a: QueryNode, b: QueryNode) -> QueryNode {
        self.binary(Binary::Difference, a, b)
    }
    pub fn sequence(&mut self, a: QueryNode, b: QueryNode) -> QueryNode {
        self.binary(Binary::Sequence, a, b)
    }
    pub fn until(&mut self, a: QueryNode, b: QueryNode) -> QueryNode {
        self.binary(Binary::Until, a, b)
    }
    pub fn star(&mut self, a: QueryNode) -> QueryNode {
        self.unary(Unary::Star, a)
    }
    pub fn complement(&mut self, a: QueryNode) -> QueryNode {
        self.unary(Unary::Complement, a)
    }
    pub fn next(&mut self, a: QueryNode) -> QueryNode {
        self.unary(Unary::Next, a)
    }
    pub fn finish(self, roots: &[QueryNode]) -> QueryProgram {
        let roots = roots.iter().map(|&root| self.index(root)).collect();
        QueryProgram(Arc::new(ProgramData {
            nodes: self.nodes,
            roots,
            max_field: self.max_field,
        }))
    }
}

struct ProgramData {
    nodes: Vec<Node>,
    roots: Vec<usize>,
    max_field: Option<u32>,
}
/// An immutable query DAG. Clones share syntax; preparing it in different engines
/// creates independent handles, compiled relations and search caches.
#[derive(Clone)]
pub struct QueryProgram(Arc<ProgramData>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum QueryError {
    NotDesugared,
    FieldOutOfRange { field: u32, fields: u32 },
    UnknownRoot(usize),
    NotPredicateRoot(usize),
    ExpansionLimit(usize),
}
impl fmt::Display for QueryError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotDesugared => write!(f, "query expressions must be desugared"),
            Self::FieldOutOfRange { field, fields } => {
                write!(f, "field {field} is outside the engine's {fields} fields")
            }
            Self::UnknownRoot(root) => write!(f, "unknown query root {root}"),
            Self::NotPredicateRoot(root) => write!(f, "query root {root} is not a predicate"),
            Self::ExpansionLimit(limit) => write!(f, "query exceeded {limit} star expansions"),
        }
    }
}
impl std::error::Error for QueryError {}

/// The expansion limit applies per query call to demand-directed star steps,
/// including nested stars. Exact automaton fallback and preparation are not
/// covered by this limit. Zero cached views disables reuse without changing answers.
#[derive(Clone, Copy, Debug)]
pub struct QueryOptions {
    pub cached_views: usize,
    pub max_star_expansions: Option<usize>,
}
impl Default for QueryOptions {
    fn default() -> Self {
        Self {
            cached_views: 64,
            max_star_expansions: None,
        }
    }
}

impl QueryProgram {
    /// Snapshot a desugared expression. Conversion and DAG traversal are iterative.
    pub fn from_expr(expr: &Expr) -> Result<Self, QueryError> {
        enum Work<'a> {
            Visit(&'a Expr),
            Unary(Unary),
            Binary(Binary),
        }
        let mut builder = QueryBuilder::new();
        let mut todo = vec![Work::Visit(expr)];
        let mut values = Vec::new();
        while let Some(work) = todo.pop() {
            match work {
                Work::Unary(op) => {
                    let a = values.pop().unwrap();
                    values.push(builder.unary(op, a));
                }
                Work::Binary(op) => {
                    let b = values.pop().unwrap();
                    let a = values.pop().unwrap();
                    values.push(builder.binary(op, a, b));
                }
                Work::Visit(expr) => {
                    let binary = match expr {
                        Expr::Union(a, b) => Some((Binary::Union, a, b)),
                        Expr::Intersect(a, b) => Some((Binary::Intersect, a, b)),
                        Expr::Xor(a, b) => Some((Binary::Xor, a, b)),
                        Expr::Difference(a, b) => Some((Binary::Difference, a, b)),
                        Expr::Sequence(a, b) => Some((Binary::Sequence, a, b)),
                        Expr::LtlUntil(a, b) => Some((Binary::Until, a, b)),
                        _ => None,
                    };
                    if let Some((op, a, b)) = binary {
                        todo.extend([Work::Binary(op), Work::Visit(b), Work::Visit(a)]);
                        continue;
                    }
                    let unary = match expr {
                        Expr::Star(a) => Some((Unary::Star, a)),
                        Expr::Complement(a) => Some((Unary::Complement, a)),
                        Expr::LtlNext(a) => Some((Unary::Next, a)),
                        _ => None,
                    };
                    if let Some((op, a)) = unary {
                        todo.extend([Work::Unary(op), Work::Visit(a)]);
                        continue;
                    }
                    let node = match expr {
                        Expr::Zero => Node::Zero,
                        Expr::One => Node::One,
                        Expr::Top => Node::Top,
                        Expr::End => Node::End,
                        Expr::Dup => Node::Dup,
                        Expr::Test(f, v) => Node::Test(*f, *v),
                        Expr::Assign(f, v) => Node::Assign(*f, *v),
                        _ => return Err(QueryError::NotDesugared),
                    };
                    values.push(builder.insert(node));
                }
            }
        }
        Ok(builder.finish(&values))
    }
    pub fn root_count(&self) -> usize {
        self.0.roots.len()
    }
    pub fn prepare<'a>(&self, aut: &'a mut Aut) -> Result<PreparedQuery<'a>, QueryError> {
        self.prepare_with_options(aut, QueryOptions::default())
    }
    pub fn prepare_with_options<'a>(
        &self,
        aut: &'a mut Aut,
        options: QueryOptions,
    ) -> Result<PreparedQuery<'a>, QueryError> {
        let fields = aut.spp.sp.num_vars();
        if let Some(field) = self.0.max_field {
            if field >= fields {
                return Err(QueryError::FieldOutOfRange { field, fields });
            }
        }
        let count = self.0.nodes.len();
        let mut references = vec![0usize; count];
        for node in &self.0.nodes {
            for child in node.children() {
                references[child] += 1;
            }
        }
        let mut query = PreparedQuery {
            references,
            aut,
            program: self.clone(),
            options,
            actions: vec![Action::Filter(SP(0)); count],
            exact: vec![None; count],
            predicates: vec![false; count],
            views: FxHashMap::default(),
            order: VecDeque::new(),
            stats: QueryStats::default(),
        };
        query.compile();
        Ok(query)
    }
}

#[derive(Clone, Copy)]
enum Action {
    Filter(SP),
    Relation(SPP),
    Union(usize, usize),
    Sequence(usize, usize),
    Star(usize),
    Alias(usize),
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Direction {
    Forward,
    Backward,
}
#[derive(Clone, Copy)]
struct View {
    seen: SP,
    frontier: SP,
    complete: bool,
}
type ViewKey = (usize, SP, Direction);

/// Mechanism counters, cumulative over calls on one prepared query.
#[derive(Clone, Copy, Debug, Default)]
pub struct QueryStats {
    pub star_expansions: usize,
    pub image_evaluations: usize,
    pub reused_views: usize,
    pub history_barriers: usize,
    pub canonical_stars: usize,
}

/// A prepared program borrowing its engine. Query roots are numbered in the
/// order passed to QueryBuilder::finish. No store-local handles cross this API.
pub struct PreparedQuery<'a> {
    aut: &'a mut Aut,
    program: QueryProgram,
    options: QueryOptions,
    actions: Vec<Action>,
    references: Vec<usize>,
    exact: Vec<Option<Compiled>>,
    predicates: Vec<bool>,
    views: FxHashMap<ViewKey, View>,
    order: VecDeque<ViewKey>,
    stats: QueryStats,
}

impl Aut {
    /// Decide emptiness without materializing all-pairs closure for positive
    /// queries. Full canonical compilation and witness APIs retain their contracts.
    pub fn is_empty_expr(&mut self, expr: &Expr) -> Result<bool, QueryError> {
        QueryProgram::from_expr(expr)?.prepare(self)?.is_empty(0)
    }
}

impl PreparedQuery<'_> {
    pub fn stats(&self) -> QueryStats {
        self.stats
    }
    /// Changing limits preserves valid cached progress from earlier calls.
    pub fn set_expansion_limit(&mut self, limit: Option<usize>) {
        self.options.max_star_expansions = limit;
    }
    pub fn clear_views(&mut self) {
        self.views.clear();
        self.order.clear();
    }
    pub fn is_empty(&mut self, root: usize) -> Result<bool, QueryError> {
        let node = *self
            .program
            .0
            .roots
            .get(root)
            .ok_or(QueryError::UnknownRoot(root))?;
        let mut remaining = self.options.max_star_expansions;
        let one = self.aut.spp.sp.one;
        Ok(!self.decide(node, one, one, &mut remaining)?)
    }
    /// Compute the complete output packet set from a predicate root.
    pub fn post(
        &mut self,
        root: usize,
        input_root: usize,
    ) -> Result<QueryPacketSet<'_>, QueryError> {
        self.packet_image(root, input_root, Direction::Forward)
    }
    /// Compute the complete input packet set reaching a predicate root.
    pub fn pre(
        &mut self,
        root: usize,
        output_root: usize,
    ) -> Result<QueryPacketSet<'_>, QueryError> {
        self.packet_image(root, output_root, Direction::Backward)
    }
    fn packet_image(
        &mut self,
        root: usize,
        set_root: usize,
        direction: Direction,
    ) -> Result<QueryPacketSet<'_>, QueryError> {
        let node = *self
            .program
            .0
            .roots
            .get(root)
            .ok_or(QueryError::UnknownRoot(root))?;
        let set = *self
            .program
            .0
            .roots
            .get(set_root)
            .ok_or(QueryError::UnknownRoot(set_root))?;
        if !self.predicates[set] {
            return Err(QueryError::NotPredicateRoot(set_root));
        }
        let Action::Filter(input) = self.actions[set] else {
            unreachable!()
        };
        let mut remaining = self.options.max_star_expansions;
        let value = self.image(node, input, direction, &mut remaining)?;
        Ok(QueryPacketSet {
            store: &self.aut.spp.sp,
            value,
        })
    }
    fn compile(&mut self) {
        let mut flat = vec![false; self.actions.len()];
        for id in 0..self.actions.len() {
            let node = self.program.0.nodes[id];
            self.predicates[id] = match node {
                Node::Zero | Node::One | Node::Test(_, _) => true,
                Node::Binary(op, a, b) if op != Binary::Until => {
                    self.predicates[a] && self.predicates[b]
                }
                _ => false,
            };
            flat[id] = match node {
                Node::Zero | Node::One | Node::Test(_, _) | Node::Assign(_, _) | Node::End => true,
                Node::Binary(op, a, b) if op != Binary::Until => flat[a] && flat[b],
                _ => false,
            };
        }
        let mut needed = vec![false; self.actions.len()];
        let mut todo = self.program.0.roots.clone();
        while let Some(id) = todo.pop() {
            if needed[id] {
                continue;
            }
            needed[id] = true;
            let node = self.program.0.nodes[id];
            self.actions[id] = if self.predicates[id] {
                let Compiled::Predicate(p) = self.exact(id) else {
                    unreachable!()
                };
                Action::Filter(p)
            } else if flat[id] && !matches!(node, Node::Binary(Binary::Sequence, _, _)) {
                let value = self.exact(id);
                Action::Relation(self.aut.compiled_to_relation(value))
            } else {
                match node {
                    Node::Dup => Action::Filter(self.aut.spp.sp.one),
                    Node::Top => Action::Relation(self.aut.spp.top),
                    Node::Binary(Binary::Union, a, b) => {
                        todo.extend([a, b]);
                        Action::Union(a, b)
                    }
                    Node::Binary(Binary::Sequence, a, b) => {
                        todo.extend([a, b]);
                        Action::Sequence(a, b)
                    }
                    Node::Unary(Unary::Star, a) => {
                        todo.push(a);
                        Action::Star(a)
                    }
                    Node::Binary(Binary::Difference, a, b)
                        if self.program.0.nodes[b] == Node::Zero =>
                    {
                        todo.push(a);
                        Action::Alias(a)
                    }
                    _ => {
                        let value = self.exact(id);
                        let state = self.aut.compiled_to_state(value);
                        self.stats.history_barriers += 1;
                        Action::Relation(self.aut.eliminate_dup(state))
                    }
                }
            };
        }
    }
    fn exact(&mut self, initial: usize) -> Compiled {
        let mut todo = vec![(initial, false)];
        while let Some((id, ready)) = todo.pop() {
            if self.exact[id].is_some() {
                continue;
            }
            let node = self.program.0.nodes[id];
            if !ready {
                todo.push((id, true));
                todo.extend(node.children().map(|child| (child, false)));
                continue;
            }
            let value = match node {
                Node::Zero => Compiled::Predicate(self.aut.spp.sp.zero),
                Node::One => Compiled::Predicate(self.aut.spp.sp.one),
                Node::Test(f, v) => Compiled::Predicate(self.aut.spp.sp.test(f, v)),
                Node::Assign(f, v) => Compiled::Relation(self.aut.spp.assign(f, v)),
                Node::End => Compiled::Relation(self.aut.spp.top),
                Node::Top => Compiled::Trace(self.aut.mk_top()),
                Node::Dup => Compiled::Trace(self.aut.mk_dup()),
                Node::Binary(op, a, b) => {
                    let a = self.exact[a].unwrap();
                    let b = self.exact[b].unwrap();
                    if let (Compiled::Predicate(x), Compiled::Predicate(y)) = (a, b) {
                        if op != Binary::Until {
                            let p = match op {
                                Binary::Union => self.aut.spp.sp.union(x, y),
                                Binary::Xor => self.aut.spp.sp.xor(x, y),
                                Binary::Difference => self.aut.spp.sp.difference(x, y),
                                _ => self.aut.spp.sp.intersect(x, y),
                            };
                            self.exact[id] = Some(Compiled::Predicate(p));
                            continue;
                        }
                    }
                    if op != Binary::Until
                        && !matches!(a, Compiled::Trace(_))
                        && !matches!(b, Compiled::Trace(_))
                    {
                        let x = self.aut.compiled_to_relation(a);
                        let y = self.aut.compiled_to_relation(b);
                        let result = match op {
                            Binary::Union => self.aut.spp.union(x, y),
                            Binary::Intersect => self.aut.spp.intersect(x, y),
                            Binary::Xor => self.aut.spp.xor(x, y),
                            Binary::Difference => self.aut.spp.difference(x, y),
                            Binary::Sequence => self.aut.spp.sequence(x, y),
                            Binary::Until => unreachable!(),
                        };
                        self.exact[id] = Some(Compiled::Relation(result));
                        continue;
                    }
                    let x = self.aut.compiled_to_state(a);
                    let y = self.aut.compiled_to_state(b);
                    let state = match op {
                        Binary::Union => self.aut.mk_union(x, y),
                        Binary::Intersect => self.aut.mk_intersect(x, y),
                        Binary::Xor => self.aut.mk_xor(x, y),
                        Binary::Difference => self.aut.mk_difference(x, y),
                        Binary::Sequence => self.aut.mk_sequence(x, y),
                        Binary::Until => self.aut.mk_until(x, y),
                    };
                    self.aut.compiled_state(state)
                }
                Node::Unary(op, a) => {
                    let a = self.exact[a].unwrap();
                    let state = self.aut.compiled_to_state(a);
                    let result = match op {
                        Unary::Star => {
                            self.stats.canonical_stars += 1;
                            self.aut.mk_star(state)
                        }
                        Unary::Complement => self.aut.mk_complement(state),
                        Unary::Next => self.aut.intern(super::AExpr::LtlNext(state)),
                    };
                    self.aut.compiled_state(result)
                }
            };
            self.exact[id] = Some(value);
        }
        self.exact[initial].unwrap()
    }
    fn remember(&mut self, key: ViewKey, view: View) {
        if self.options.cached_views == 0 {
            return;
        }
        if !self.views.contains_key(&key) {
            if self.views.len() == self.options.cached_views {
                self.views.remove(&self.order.pop_front().unwrap());
            }
            self.order.push_back(key);
        }
        self.views.insert(key, view);
    }
    fn image(
        &mut self,
        node: usize,
        input: SP,
        direction: Direction,
        remaining: &mut Option<usize>,
    ) -> Result<SP, QueryError> {
        self.evaluate(node, input, direction, None, remaining)
    }
    fn evaluate(
        &mut self,
        node: usize,
        input: SP,
        direction: Direction,
        goal: Option<SP>,
        remaining: &mut Option<usize>,
    ) -> Result<SP, QueryError> {
        enum Work {
            Eval(usize, SP, Option<SP>),
            Remember(usize, SP),
            Union,
            Then(usize),
            Star(ViewKey, usize, View, Option<SP>),
            Advance(ViewKey, usize, View, Option<SP>),
        }
        let mut todo = vec![Work::Eval(node, input, goal)];
        let mut images = FxHashMap::default();
        let mut values = Vec::new();
        while let Some(work) = todo.pop() {
            match work {
                Work::Remember(node, input) => {
                    images.insert((node, input), *values.last().unwrap());
                }
                Work::Eval(node, input, goal) => {
                    if goal.is_none() && self.references[node] > 1 {
                        if let Some(&result) = images.get(&(node, input)) {
                            values.push(result);
                            continue;
                        }
                        todo.push(Work::Remember(node, input));
                    }
                    self.stats.image_evaluations += 1;
                    if self.aut.spp.sp.is_zero(input) {
                        values.push(input);
                        continue;
                    }
                    match self.actions[node] {
                        Action::Filter(p) => values.push(self.aut.spp.sp.intersect(input, p)),
                        Action::Relation(r) => values.push(match direction {
                            Direction::Forward => self.aut.spp.push(input, r),
                            Direction::Backward => self.aut.spp.pull(r, input),
                        }),
                        Action::Alias(a) => todo.push(Work::Eval(a, input, goal)),
                        Action::Union(a, b) => todo.extend([
                            Work::Union,
                            Work::Eval(b, input, None),
                            Work::Eval(a, input, None),
                        ]),
                        Action::Sequence(a, b) => {
                            let (a, b) = match direction {
                                Direction::Forward => (a, b),
                                Direction::Backward => (b, a),
                            };
                            todo.extend([Work::Then(b), Work::Eval(a, input, None)]);
                        }
                        Action::Star(body) => {
                            let key = (node, input, direction);
                            let view = match self.views.get(&key).copied() {
                                Some(view) => {
                                    self.stats.reused_views += 1;
                                    view
                                }
                                None => View {
                                    seen: input,
                                    frontier: input,
                                    complete: false,
                                },
                            };
                            todo.push(Work::Star(key, body, view, goal));
                        }
                    }
                }
                Work::Union => {
                    let b = values.pop().unwrap();
                    let a = values.pop().unwrap();
                    values.push(self.aut.spp.sp.union(a, b));
                }
                Work::Then(node) => todo.push(Work::Eval(node, values.pop().unwrap(), None)),
                Work::Star(key, body, view, goal) => {
                    let hit = goal.map(|p| self.aut.spp.sp.intersect(view.seen, p));
                    if view.complete || hit.is_some_and(|p| !self.aut.spp.sp.is_zero(p)) {
                        values.push(view.seen);
                        continue;
                    }
                    if let Some(left) = remaining {
                        if *left == 0 {
                            return Err(QueryError::ExpansionLimit(
                                self.options.max_star_expansions.unwrap(),
                            ));
                        }
                        *left -= 1;
                    }
                    self.stats.star_expansions += 1;
                    todo.extend([
                        Work::Advance(key, body, view, goal),
                        Work::Eval(body, view.frontier, None),
                    ]);
                }
                Work::Advance(key, body, view, goal) => {
                    let fresh = self.aut.spp.sp.difference(values.pop().unwrap(), view.seen);
                    let view = View {
                        seen: self.aut.spp.sp.union(view.seen, fresh),
                        frontier: fresh,
                        complete: self.aut.spp.sp.is_zero(fresh),
                    };
                    // Publish only completed expansions. Errors preserve the last frontier.
                    self.remember(key, view);
                    todo.push(Work::Star(key, body, view, goal));
                }
            }
        }
        Ok(values.pop().unwrap())
    }
    fn decide(
        &mut self,
        node: usize,
        input: SP,
        output: SP,
        remaining: &mut Option<usize>,
    ) -> Result<bool, QueryError> {
        let mut todo = vec![(node, input, output)];
        let mut visited = rustc_hash::FxHashSet::default();
        while let Some((node, input, output)) = todo.pop() {
            if !visited.insert((node, input, output)) {
                continue;
            }
            if self.aut.spp.sp.is_zero(input) || self.aut.spp.sp.is_zero(output) {
                continue;
            }
            match self.actions[node] {
                Action::Alias(a) => todo.push((a, input, output)),
                Action::Union(a, b) => todo.extend([(b, input, output), (a, input, output)]),
                Action::Sequence(_, _) => {
                    let mut pending = vec![node];
                    let mut factors = Vec::new();
                    let mut expanded = rustc_hash::FxHashSet::default();
                    while let Some(id) = pending.pop() {
                        if let Action::Sequence(a, b) = self.actions[id]
                            && (self.references[id] <= 1 || expanded.insert(id))
                        {
                            pending.push(b);
                            pending.push(a);
                        } else {
                            factors.push(id);
                        }
                    }
                    let pivot = factors
                        .iter()
                        .position(|&id| matches!(self.actions[id], Action::Star(_)))
                        .unwrap_or(factors.len() - 1);
                    let mut seed = input;
                    for &id in &factors[..pivot] {
                        seed = self.image(id, seed, Direction::Forward, remaining)?;
                    }
                    let mut goal = output;
                    for &id in factors[pivot + 1..].iter().rev() {
                        goal = self.image(id, goal, Direction::Backward, remaining)?;
                    }
                    todo.push((factors[pivot], seed, goal));
                }
                _ => {
                    let goal = matches!(self.actions[node], Action::Star(_)).then_some(output);
                    let reachable =
                        self.evaluate(node, input, Direction::Forward, goal, remaining)?;
                    let hit = self.aut.spp.sp.intersect(reachable, output);
                    if !self.aut.spp.sp.is_zero(hit) {
                        return Ok(true);
                    }
                }
            }
        }
        Ok(false)
    }
}

/// A packet-set view tied to the borrowed engine. It exposes no raw handles.
pub struct QueryPacketSet<'a> {
    store: &'a crate::sp::SPstore,
    value: SP,
}
impl QueryPacketSet<'_> {
    pub fn num_fields(&self) -> usize {
        self.store.num_vars() as usize
    }
    pub fn is_empty(&self) -> bool {
        self.value == self.store.zero
    }
    /// Test membership. Packets must have exactly num_fields() bits.
    pub fn contains(&self, packet: &[bool]) -> bool {
        assert_eq!(
            packet.len(),
            self.num_fields(),
            "packet width does not match query engine"
        );
        let mut node = self.value;
        for &bit in packet {
            let children = self.store.get(node);
            node = if bit { children.x1 } else { children.x0 };
        }
        node == SP(1)
    }
}
