use crate::expr::Expr;
use crate::spp;
use rustc_hash::FxHashMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::hash::Hash;
// An AExpr represents an automaton state.
// This is essentially a compressed and hash-consed form of a NetKAT expression.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum AExpr {
    SPP(spp::SPP), // We keep field tests and mutations and combinations thereof in SPP form
    Union(Vec<State>), // e1 + e2 + ... + en
    Intersect(Vec<State>), // e1 & e2 & ... & en
    Xor(State, State), // e1 ^ e2
    Difference(State, State), // e1 - e2
    Complement(State), // !e1
    Sequence(State, State), // e1; e2
    Star(State),   // e*
    Dup,           // dup
    LtlNext(State), // X e
    LtlUntil(State, State), // e1 U e2
    Top,           // represents the set of all strings
}

// A State is an index into the Aut's expression table.
type State = usize;

/// Symbolic transitions ST<T>.           
/// Symbolic transitions represent, for each T, a set of packet pairs that can transition to T. These are represented as a finite map from T to SPP's.
/// A symbolic transition can be deterministic or nondeterministic, depending on whether the SPPs associated with different T's are disjoint. We typically keep ST's in deterministic form.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ST {
    transitions: HashMap<State, spp::SPP>,
}

impl ST {
    pub fn new(transitions: HashMap<State, spp::SPP>) -> Self {
        ST { transitions }
    }
    pub fn empty() -> Self {
        ST {
            transitions: HashMap::new(),
        }
    }

    /// Returns a reference to the internal transitions map
    pub fn get_transitions(&self) -> &HashMap<State, spp::SPP> {
        &self.transitions
    }
}

#[derive(Clone, Copy)]
enum Compiled {
    Predicate(crate::sp::SP),
    Relation(spp::SPP),
    Trace(State),
}

pub struct Aut {
    aexprs: Vec<AExpr>,
    aexpr_map: FxHashMap<AExpr, State>,
    spp_states: FxHashMap<spp::SPP, State>,
    delta_map: FxHashMap<State, ST>,
    epsilon_map: FxHashMap<State, spp::SPP>,
    eliminate_dup_cache: FxHashMap<State, spp::SPP>,
    spp: spp::SPPstore,
    // num_vars: u32,
    live_cache: FxHashMap<State, crate::sp::SP>,
    seen: Vec<(u64, crate::sp::SP)>,
    query_epoch: u64,
    root_scratch: Vec<u64>,
    root_snapshots: Vec<(u64, Box<[u64]>, State)>,
}

impl Aut {
    pub fn new(num_vars: u32) -> Self {
        let aut = Aut {
            aexprs: vec![],
            aexpr_map: FxHashMap::default(),
            spp_states: FxHashMap::default(),
            delta_map: FxHashMap::default(),
            epsilon_map: FxHashMap::default(),
            eliminate_dup_cache: FxHashMap::default(),
            spp: spp::SPPstore::new(num_vars),
            // num_vars,
            live_cache: FxHashMap::default(),
            seen: Vec::new(),
            query_epoch: 0,
            root_scratch: Vec::new(),
            root_snapshots: Vec::new(),
        };
        aut
    }

    // --- States ---

    // Internal function to hash-cons an expression
    fn intern(&mut self, expr: AExpr) -> State {
        if let Some(&id) = self.aexpr_map.get(&expr) {
            return id;
        }
        let id = self.aexprs.len();
        self.aexprs.push(expr.clone());
        self.aexpr_map.insert(expr, id);
        id
    }

    // Smart Constructors with Simplifications

    fn mk_spp(&mut self, spp: spp::SPP) -> State {
        *self.spp_states.entry(spp).or_insert_with(|| {
            let state = self.aexprs.len();
            self.aexprs.push(AExpr::SPP(spp));
            state
        })
    }

    fn mk_union_n(&mut self, states: Vec<State>) -> State {
        if states.len() == 1 { return states[0]; }
        let mut states2 = vec![];
        for state in states {
            match self.get_expr(state) {
                AExpr::Union(nested) => {
                    states2.extend_from_slice(nested);
                }
                AExpr::Top => {
                    return self.mk_top();
                }
                _ => states2.push(state),
            }
        }
        let mut spp = self.spp.zero;
        let mut new_states = vec![];
        for state in states2 {
            match self.get_expr(state) {
                AExpr::SPP(s) => spp = self.spp.union(spp, *s),
                _ => new_states.push(state),
            }
        }
        if spp != self.spp.zero {
            new_states.push(self.mk_spp(spp));
        }

        new_states.sort();
        new_states.dedup();
        // Special case: just one element
        if new_states.len() == 1 {
            return new_states[0];
        }
        if new_states.is_empty() {
            return self.mk_spp(self.spp.zero);
        }

        // Create an n-ary union
        self.intern(AExpr::Union(new_states))
    }

    fn mk_union(&mut self, e1: State, e2: State) -> State {
        if e1 == e2 { return e1; }
        if let (AExpr::SPP(a), AExpr::SPP(b)) = (self.get_expr(e1), self.get_expr(e2)) {
            let result = self.spp.union(*a, *b);
            return self.mk_spp(result);
        }
        self.mk_union_n(vec![e1, e2])
    }

    fn mk_intersect_n(&mut self, states: Vec<State>) -> State {
        if states.len() == 1 { return states[0]; }
        let mut new_states = vec![];
        for state in states {
            match self.get_expr(state) {
                AExpr::Intersect(nested) => {
                    new_states.extend_from_slice(nested);
                }
                _ => new_states.push(state),
            }
        }

        // Distribute intersections over unions
        let mut distributed_states: Vec<Vec<State>> = vec![vec![]];
        for state in new_states {
            match self.get_expr(state) {
                AExpr::Union(nested) => {
                    let mut new_distributed_states = vec![];
                    for &nested_state in nested {
                        for i in 0..distributed_states.len() {
                            let mut new_distributed_state = distributed_states[i].clone();
                            new_distributed_state.push(nested_state);
                            new_distributed_states.push(new_distributed_state);
                        }
                    }
                    distributed_states = new_distributed_states;
                }
                _ => {
                    for i in 0..distributed_states.len() {
                        distributed_states[i].push(state);
                    }
                }
            }
        }

        // Make an actual intersection of each distributed state
        let mut intersections = vec![];
        for distributed_state in distributed_states {
            intersections.push(self.mk_intersect_n_base(distributed_state));
        }

        return self.mk_union_n(intersections);
    }

    fn mk_intersect_n_base(&mut self, mut states: Vec<State>) -> State {
        // Basic version that does not do distribution, but does handle Top and Zero and merges SPPs
        let mut new_states = vec![];
        for state in states {
            match self.get_expr(state) {
                AExpr::Intersect(nested) => {
                    new_states.extend_from_slice(nested);
                }
                _ => new_states.push(state),
            }
        }
        states = new_states;
        // Remove Top
        states.retain(|&state| !matches!(self.get_expr(state), AExpr::Top));
        // Intersect the SPPs, collecting the rest
        let mut spp = None;
        let mut rest = vec![];
        for state in states {
            match self.get_expr(state) {
                AExpr::SPP(s) => spp = Some(self.spp.intersect(spp.unwrap_or(self.spp.top), *s)),
                _ => rest.push(state),
            }
        }
        rest.sort();
        rest.dedup();
        if spp == Some(self.spp.zero) {
            return self.mk_spp(self.spp.zero);
        }
        if rest.is_empty() {
            if let Some(spp) = spp {
                return self.mk_spp(spp);
            } else {
                return self.mk_top();
            }
        }
        if let Some(spp) = spp {
            rest.push(self.mk_spp(spp));
        }
        self.intern(AExpr::Intersect(rest))
    }

    fn mk_intersect(&mut self, e1: State, e2: State) -> State {
        if e1 == e2 { return e1; }
        if let (AExpr::SPP(a), AExpr::SPP(b)) = (self.get_expr(e1), self.get_expr(e2)) {
            let result = self.spp.intersect(*a, *b);
            return self.mk_spp(result);
        }
        self.mk_intersect_n(vec![e1, e2])
    }

    fn mk_xor(&mut self, e1: State, e2: State) -> State {
        if e1 == e2 {
            return self.mk_spp(self.spp.zero);
        } // e ^ e = 0

        // SPP Simplification: s1 ^ s2
        if let (AExpr::SPP(s1), AExpr::SPP(s2)) = (self.get_expr(e1), self.get_expr(e2)) {
            let result_spp = self.spp.xor(*s1, *s2);
            return self.mk_spp(result_spp);
        }

        // Canonical ordering
        let (e1, e2) = if e1 < e2 { (e1, e2) } else { (e2, e1) };
        self.intern(AExpr::Xor(e1, e2))
    }

    pub fn mk_difference(&mut self, e1: State, e2: State) -> State {
        if e1 == e2 {
            return self.mk_spp(self.spp.zero);
        } // e - e = 0

        // SPP Simplification: s1 - s2
        if let (AExpr::SPP(s1), AExpr::SPP(s2)) = (self.get_expr(e1), self.get_expr(e2)) {
            let result_spp = self.spp.difference(*s1, *s2);
            return self.mk_spp(result_spp);
        }

        self.intern(AExpr::Difference(e1, e2))
    }

    fn mk_complement(&mut self, e: State) -> State {
        // Simplify !!e = e
        if let AExpr::Complement(inner_e) = self.get_expr(e) {
            return *inner_e;
        }

        // // De Morgan's laws and complement simplifications
        let expr = self.get_expr(e).clone();
        // match expr {
        //     AExpr::Union(states) => {
        //         // !(e1 + e2 + ... + en) = !e1 & !e2 & ... & !en
        //         let complements: Vec<State> = states
        //             .iter()
        //             .map(|&state| self.mk_complement(state))
        //             .collect();
        //         return self.mk_intersect_n(complements);
        //     }
        //     AExpr::Intersect(states) => {
        //         // !(e1 & e2 & ... & en) = !e1 + !e2 + ... + !en
        //         let complements: Vec<State> = states
        //             .iter()
        //             .map(|&state| self.mk_complement(state))
        //             .collect();
        //         return self.mk_union_n(complements);
        //     }
        //     AExpr::Difference(e1, e2) => {
        //         let c1 = self.mk_complement(e1);
        //         return self.mk_union(c1, e2);
        //     }
        //     AExpr::Xor(e1, e2) => {
        //         let c1 = self.mk_complement(e1);
        //         return self.mk_xor(c1, e2);
        //     }
        //     _ => {}
        // }

        // Simplify !⊤ = 0
        if let AExpr::Top = expr {
            return self.mk_spp(self.spp.zero);
        }

        // SPP Simplification is not valid here, since we are complementing a set of strings!
        self.intern(AExpr::Complement(e))
    }

    fn mk_sequence(&mut self, e1: State, e2: State) -> State {
        // SPP Simplification: s1 ; s2
        if let (AExpr::SPP(s1), AExpr::SPP(s2)) = (self.get_expr(e1), self.get_expr(e2)) {
            let result_spp = self.spp.sequence(*s1, *s2);
            return self.mk_spp(result_spp);
        }
        // Simplify SPP.one ; e = e and e ; SPP.one = e and SPP.zero ; e = SPP.zero and e ; SPP.zero = SPP.zero
        match self.get_expr(e1) {
            AExpr::SPP(s1) => {
                if *s1 == self.spp.one {
                    return e2;
                }
                if *s1 == self.spp.zero {
                    return self.mk_spp(self.spp.zero);
                }
            }
            _ => {}
        }
        match self.get_expr(e2) {
            AExpr::SPP(s2) => {
                if *s2 == self.spp.one {
                    return e1;
                }
                if *s2 == self.spp.zero {
                    return self.mk_spp(self.spp.zero);
                }
            }
            _ => {}
        }

        // (a; b); c = a; (b; c)
        if let AExpr::Sequence(e11, e12) = self.get_expr(e1).clone() {
            let tmp = self.mk_sequence(e12, e2);
            return self.mk_sequence(e11, tmp);
        }

        // Simplify T; T = T
        if let (AExpr::Top, AExpr::Top) = (self.get_expr(e1), self.get_expr(e2)) {
            return self.mk_top();
        }

        self.intern(AExpr::Sequence(e1, e2))
    }

    fn mk_star(&mut self, e: State) -> State {
        // Simplify (e*)* = e*
        if let AExpr::Star(_inner_e) = self.get_expr(e) {
            // Return the existing e* index
            return e;
        }

        // SPP Simplification: s*
        if let AExpr::SPP(s) = self.get_expr(e) {
            let result_spp = self.spp.star(*s);
            return self.mk_spp(result_spp);
        }

        // Simplify T* = T
        if let AExpr::Top = self.get_expr(e) {
            return self.mk_top();
        }

        self.intern(AExpr::Star(e))
    }

    fn mk_dup(&mut self) -> State {
        self.intern(AExpr::Dup)
    }

    fn mk_top(&mut self) -> State {
        self.intern(AExpr::Top)
    }

    fn mk_until(&mut self, e1: State, e2: State) -> State {
        self.intern(AExpr::LtlUntil(e1, e2))
    }

    // Helper to get the actual expression from an index
    fn get_expr(&self, id: State) -> &AExpr {
        &self.aexprs[id]
    }

    pub fn expr_to_state(&mut self, expr: &Expr) -> State {
        use std::hash::Hasher;
        self.root_scratch.clear();
        let cacheable = Self::encode_core(expr, &mut self.root_scratch);
        let mut hash = rustc_hash::FxHasher::default();
        for &word in &self.root_scratch { hash.write_u64(word); }
        let hash = hash.finish();
        if cacheable {
            for (key, syntax, state) in &self.root_snapshots {
                if *key == hash && syntax.as_ref() == self.root_scratch.as_slice() { return *state; }
            }
        }
        let state = self.compile_to_state(expr);
        if cacheable && self.root_scratch.len() <= 65536 {
            while !self.root_snapshots.is_empty()
                && (self.root_snapshots.len() >= 16 || self.root_snapshots.iter().map(|x| x.1.len()).sum::<usize>() + self.root_scratch.len() > 524288)
            {
                self.root_snapshots.remove(0);
            }
            self.root_snapshots.push((hash, self.root_scratch.clone().into_boxed_slice(), state));
        }
        state
    }

    // Prefix tags have fixed arities; field and value bits cannot overlap tags.
    // Admission uses exact equality because hashes alone cannot identify syntax.
    fn encode_core(expr: &Expr, words: &mut Vec<u64>) -> bool {
        use Expr::*;
        if words.len() >= 65536 { return false; }
        let tag = match expr {
            Zero => 0, One => 1, Top => 2, End => 3, Dup => 4,
            Test(field, value) => 5 | (u64::from(*value) << 7) | (u64::from(*field) << 8),
            Assign(field, value) => 6 | (u64::from(*value) << 7) | (u64::from(*field) << 8),
            Union(_,_) => 7, Intersect(_,_) => 8, Xor(_,_) => 9, Difference(_,_) => 10,
            Sequence(_,_) => 11, Star(_) => 12, Complement(_) => 13, TestNegation(_) => 14,
            LtlNext(_) => 15, LtlUntil(_,_) => 16,
            _ => return false,
        };
        words.push(tag);
        match expr {
            Union(a,b) | Intersect(a,b) | Xor(a,b) | Difference(a,b) | Sequence(a,b) | LtlUntil(a,b) => Self::encode_core(a,words) && Self::encode_core(b,words),
            Star(x) | Complement(x) | TestNegation(x) | LtlNext(x) => Self::encode_core(x,words),
            _ => true,
        }
    }

    fn compile_to_state(&mut self, expr: &Expr) -> State {
        let result = self.compile_expr(expr);
        self.compiled_to_state(result)
    }

    fn compiled_to_state(&mut self, result: Compiled) -> State {
        match result {
            Compiled::Predicate(predicate) => {
                let relation = self.spp.diagonal(predicate);
                self.mk_spp(relation)
            }
            Compiled::Relation(relation) => self.mk_spp(relation),
            Compiled::Trace(state) => state,
        }
    }

    fn compiled_to_relation(&mut self, result: Compiled) -> spp::SPP {
        match result {
            Compiled::Predicate(predicate) => self.spp.diagonal(predicate),
            Compiled::Relation(relation) => relation,
            Compiled::Trace(_) => unreachable!(),
        }
    }

    fn compiled_state(&self, state: State) -> Compiled {
        match self.get_expr(state) {
            AExpr::SPP(relation) => Compiled::Relation(*relation),
            _ => Compiled::Trace(state),
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> Compiled {
        match expr {
            Expr::Zero => Compiled::Predicate(self.spp.sp.zero),
            Expr::One => Compiled::Predicate(self.spp.sp.one),
            Expr::End => Compiled::Relation(self.spp.top),
            Expr::Test(field, value) => Compiled::Predicate(self.spp.sp.test(*field, *value)),
            Expr::Assign(field, value) => Compiled::Relation(self.spp.assign(*field, *value)),
            Expr::Union(left, right) | Expr::Intersect(left, right) | Expr::Xor(left, right)
            | Expr::Difference(left, right) | Expr::Sequence(left, right) => {
                let left = self.compile_expr(left);
                let right = self.compile_expr(right);
                if let (Compiled::Predicate(a), Compiled::Predicate(b)) = (left, right) {
                    let result = match expr {
                        Expr::Union(_, _) => self.spp.sp.union(a, b),
                        Expr::Xor(_, _) => self.spp.sp.xor(a, b),
                        Expr::Difference(_, _) => self.spp.sp.difference(a, b),
                        _ => self.spp.sp.intersect(a, b),
                    };
                    Compiled::Predicate(result)
                } else if !matches!(left, Compiled::Trace(_)) && !matches!(right, Compiled::Trace(_)) {
                    let a = self.compiled_to_relation(left);
                    let b = self.compiled_to_relation(right);
                    let result = match expr {
                        Expr::Union(_, _) => self.spp.union(a, b),
                        Expr::Intersect(_, _) => self.spp.intersect(a, b),
                        Expr::Xor(_, _) => self.spp.xor(a, b),
                        Expr::Difference(_, _) => self.spp.difference(a, b),
                        Expr::Sequence(_, _) => self.spp.sequence(a, b),
                        _ => unreachable!(),
                    };
                    Compiled::Relation(result)
                } else {
                    let a = self.compiled_to_state(left);
                    let b = self.compiled_to_state(right);
                    let state = match expr {
                        Expr::Union(_, _) => self.mk_union(a, b),
                        Expr::Intersect(_, _) => self.mk_intersect(a, b),
                        Expr::Xor(_, _) => self.mk_xor(a, b),
                        Expr::Difference(_, _) => self.mk_difference(a, b),
                        Expr::Sequence(_, _) => self.mk_sequence(a, b),
                        _ => unreachable!(),
                    };
                    self.compiled_state(state)
                }
            }
            Expr::Star(inner) => match self.compile_expr(inner) {
                Compiled::Predicate(_) => Compiled::Predicate(self.spp.sp.one),
                Compiled::Relation(relation) => Compiled::Relation(self.spp.star(relation)),
                Compiled::Trace(state) => {
                    let state = self.mk_star(state);
                    self.compiled_state(state)
                }
            },
            Expr::Complement(inner) => {
                let state = self.compile_to_state(inner);
                let state = self.mk_complement(state);
                self.compiled_state(state)
            }
            Expr::Top => Compiled::Trace(self.mk_top()),
            Expr::Dup => Compiled::Trace(self.mk_dup()),
            Expr::LtlNext(inner) => {
                let state = self.compile_to_state(inner);
                Compiled::Trace(self.intern(AExpr::LtlNext(state)))
            }
            Expr::LtlUntil(left, right) => {
                let a = self.compile_to_state(left);
                let b = self.compile_to_state(right);
                Compiled::Trace(self.intern(AExpr::LtlUntil(a, b)))
            }
            Expr::TestNegation(_) => panic!("TestNegation should have been eliminated during desugaring"),
            Expr::IfThenElse(_, _, _) => panic!("IfThenElse should have been eliminated during desugaring"),
            Expr::Var(_) => panic!("Variables should have been eliminated during desugaring"),
            Expr::Let(_, _, _) => panic!("Let expressions should have been eliminated during desugaring"),
            Expr::LetBitRange(_, _, _, _) => panic!("LetBitRange expressions should have been eliminated during desugaring"),
            Expr::VarAssign(_, _) => panic!("VarAssign expressions should have been eliminated during desugaring"),
            Expr::VarTest(_, _) => panic!("VarTest expressions should have been eliminated during desugaring"),
            Expr::BitRangeAssign(_, _, _) => panic!("BitRangeAssign should have been eliminated during desugaring"),
            Expr::BitRangeTest(_, _, _) => panic!("BitRangeTest should have been eliminated during desugaring"),
            Expr::BitRangeMatch(_, _, _) => panic!("BitRangeMatch should have been eliminated during desugaring"),
            Expr::VarMatch(_, _) => panic!("VarMatch should have been eliminated during desugaring"),
        }
    }

    // --- Symbolic transitions: ST ---

    /// The empty ST
    pub fn st_empty(&mut self) -> ST {
        ST::new(HashMap::new())
    }

    /// Creates a singleton ST mapping `spp` to `state`
    pub fn st_singleton(&mut self, spp: spp::SPP, state: State) -> ST {
        if spp == self.spp.zero {
            return ST::new(HashMap::new());
        }
        if state == self.mk_spp(self.spp.zero) {
            return ST::new(HashMap::new());
        }
        ST::new(HashMap::from([(state, spp)]))
    }

    // /// Insert a transition into a ST.
    // /// Precondition: spp is disjoint from all other spp's in the ST
    // pub fn st_insert_unsafe(&mut self, st: &mut ST, state: State, spp: spp::SPP) {
    //     // Assert that spp is disjoint from all other spp's in the ST
    //     #[cfg(debug_assertions)]
    //     for (_, existing_spp) in st.transitions.iter() {
    //         debug_assert!(
    //             self.spp.intersect(*existing_spp, spp) == self.spp.zero,
    //             "spp must be disjoint from all other spp's in the ST"
    //         );
    //     }
    //     // Check if the state already exists, if so union the spp's
    //     if let Some(existing_spp) = st.transitions.get_mut(&state) {
    //         *existing_spp = self.spp.union(*existing_spp, spp);
    //     } else {
    //         // Check if the spp is 0, if so don't insert
    //         if spp == self.spp.zero {
    //             return;
    //         }
    //         // Check if the State is zero, if so don't insert
    //         if state == self.mk_spp(self.spp.zero) {
    //             return;
    //         }
    //         st.transitions.insert(state, spp);
    //     }
    // }

    fn st_insert_helper(&mut self, st: &mut ST, state: State, spp: spp::SPP) {
        if spp == self.spp.zero {
            return;
        }
        if state == self.mk_spp(self.spp.zero) {
            return;
        }
        if let Some(existing_spp) = st.transitions.get_mut(&state) {
            *existing_spp = self.spp.union(*existing_spp, spp);
        } else {
            st.transitions.insert(state, spp);
        }
    }

    /// Insert a transition into a ST.
    pub fn st_insert(&mut self, st: &mut ST, state: State, spp: spp::SPP) {
        // We have to be careful here because a naive implementation would not result in a deterministic ST
        // Strategy: intersect the spp with all other spp's in the ST, and insert an expr union for those
        // Separately keep track of the remaining spp that is inserted separately

        let mut result = ST::empty();
        let mut total_instersect = self.spp.zero;
        for (&state2, &spp2) in &st.transitions {
            let intersect_spp = self.spp.intersect(spp, spp2);
            let union_state = self.mk_union(state, state2);
            self.st_insert_helper(&mut result, union_state, intersect_spp);
            total_instersect = self.spp.union(total_instersect, intersect_spp);
            let diff_spp = self.spp.difference(spp2, intersect_spp);
            self.st_insert_helper(&mut result, state2, diff_spp);
        }

        let remaining_spp = self.spp.difference(spp, total_instersect);
        self.st_insert_helper(&mut result, state, remaining_spp);
        *st = result;
    }

    fn st_intersect(&mut self, st1: ST, st2: ST) -> ST {
        let mut result = ST::empty();
        for (state1, spp1) in &st1.transitions {
            for (state2, spp2) in &st2.transitions {
                let intersect_state = self.mk_intersect(*state1, *state2);
                let spp = self.spp.intersect(*spp1, *spp2);
                self.st_insert(&mut result, intersect_state, spp);
            }
        }
        result
    }

    pub fn st_union(&mut self, st1: ST, st2: ST) -> ST {
        let mut result = ST::empty();
        for (state, spp) in st1.transitions {
            self.st_insert(&mut result, state, spp);
        }
        for (state, spp) in st2.transitions {
            self.st_insert(&mut result, state, spp);
        }
        result
    }

    fn st_difference(&mut self, st1: ST, st2: ST) -> ST {
        // (st1 - st2) = st1 & !st2
        // let st2_complement = self.st_complement(st2);
        // self.st_intersect(st1, st2_complement)
        let mut result = ST::empty();
        let mut spp_sum = self.spp.zero;
        for (state1, spp1) in st1.transitions.clone() {
            for (state2, spp2) in st2.transitions.clone() {
                let diff_state = self.mk_difference(state1, state2);
                let inter_spp = self.spp.intersect(spp1, spp2);
                spp_sum = self.spp.union(spp_sum, inter_spp);
                self.st_insert(&mut result, diff_state, inter_spp);
            }
        }
        for (state, spp) in st1.transitions.clone() {
            let diff_spp = self.spp.difference(spp, spp_sum);
            self.st_insert(&mut result, state, diff_spp);
        }
        result
    }

    fn st_xor(&mut self, st1: ST, st2: ST) -> ST {
        // (st1 ^ st2) = (st1 - st2) + (st2 - st1)
        let st1_minus_st2 = self.st_difference(st1.clone(), st2.clone());
        let st2_minus_st1 = self.st_difference(st2, st1);
        self.st_union(st1_minus_st2, st2_minus_st1)
    }

    fn st_complement(&mut self, st: ST) -> ST {
        let mut result = ST::empty();
        for (&state, &spp) in &st.transitions {
            let new_state = self.mk_complement(state);
            self.st_insert(&mut result, new_state, spp);
        }
        // Find the union of all the spp's in the transitions
        let mut union_spp = self.spp.zero;
        for (_, &spp) in &st.transitions {
            union_spp = self.spp.union(union_spp, spp);
        }
        // Add a transition from the complement of the union to the Top state
        let complement_spp = self.spp.complement(union_spp);
        let top = self.mk_top();
        self.st_insert(&mut result, top, complement_spp);
        result
    }

    fn st_postcompose(&mut self, st: ST, expr: State) -> ST {
        let mut result = ST::empty();
        for (state, spp) in st.transitions {
            let new_state = self.mk_sequence(state, expr);
            self.st_insert(&mut result, new_state, spp);
        }
        result
    }

    fn st_precompose(&mut self, spp: spp::SPP, st: ST) -> ST {
        // Here we have to be careful because a naive implementation would not result in a deterministic ST
        // Therefore we use st_insert and not st_insert_unsafe
        let mut result = ST::empty();
        for (state, spp2) in st.transitions {
            let new_spp = self.spp.sequence(spp, spp2);
            self.st_insert(&mut result, state, new_spp);
        }
        result
    }

    /// Helper function for intersecting an ST with an `expr` on the right
    /// (used for computing the derivative of `e1 U e2`)
    fn st_intersect_expr(&mut self, st: ST, expr: State) -> ST {
        let mut result = ST::empty();
        for (state, spp) in st.transitions {
            let new_state = self.mk_intersect(state, expr);
            self.st_insert(&mut result, new_state, spp);
        }
        result
    }

    // --- Automaton construction: delta, epsilon ---

    pub fn delta(&mut self, state: State) -> ST {
        if let Some(st) = self.delta_map.get(&state) {
            return st.clone();
        }

        // Extract all needed information from the expr before recursive calls
        let expr = self.get_expr(state).clone();

        // Calculate delta for each case
        let result = match expr {
            AExpr::SPP(_) => ST::empty(),
            AExpr::Union(states) => {
                let states_copy = states.clone();
                let mut result = ST::empty();
                for s in states_copy {
                    let delta_state = self.delta(s);
                    result = self.st_union(result, delta_state);
                }
                result
            }
            AExpr::Intersect(states) => {
                // Compute all delta values first to avoid borrow issues
                let delta_values: Vec<ST> = states.iter().map(|&s| self.delta(s)).collect();

                // Then combine them with intersection
                if delta_values.is_empty() {
                    // Empty intersection is ST mapping all states to top (opposite of union's empty case)
                    // In practice, we don't expect to hit this case
                    ST::empty()
                } else {
                    // Combine using intersection
                    let mut result = delta_values[0].clone();
                    for delta in &delta_values[1..] {
                        result = self.st_intersect(result, delta.clone());
                    }
                    result
                }
            }
            AExpr::Xor(e1, e2) => {
                let delta1 = self.delta(e1);
                let delta2 = self.delta(e2);
                self.st_xor(delta1, delta2)
            }
            AExpr::Difference(e1, e2) => {
                let delta1 = self.delta(e1);
                let delta2 = self.delta(e2);
                self.st_difference(delta1, delta2)
            }
            AExpr::Complement(e) => {
                let delta_e = self.delta(e);
                self.st_complement(delta_e)
            }
            AExpr::Sequence(e1, e2) => {
                // delta(e1 e2) = delta(e1) e2 + epsilon(e1) delta(e2)
                let epsilon_e1 = self.epsilon(e1);
                let delta_e1 = self.delta(e1);
                let delta_e2 = self.delta(e2);
                let delta_e1_seq_e2 = self.st_postcompose(delta_e1, e2);
                let epsilon_e1_seq_e2 = self.st_precompose(epsilon_e1, delta_e2);
                self.st_union(delta_e1_seq_e2, epsilon_e1_seq_e2)
            }
            AExpr::Star(e) => {
                // delta(e*) = epsilon(e)* delta(e) e*
                let epsilon_e = self.epsilon(e);
                let epsilon_e_star = self.spp.star(epsilon_e);
                let delta_e = self.delta(e);
                let delta_e_star_e = self.st_postcompose(delta_e, state);
                self.st_precompose(epsilon_e_star, delta_e_star_e)
            }
            AExpr::Dup => {
                let spp_one = self.mk_spp(self.spp.one);
                self.st_singleton(self.spp.one, spp_one)
            }
            AExpr::LtlNext(e) => self.st_singleton(self.spp.top, e),
            AExpr::LtlUntil(e1, e2) => {
                // delta(e1 U e2) = delta(e2) ∪ (delta(e1) ∩ (e1 U e2))
                let delta_e1 = self.delta(e1);
                let delta_e2 = self.delta(e2);
                let e1_u_e2 = self.mk_until(e1, e2);
                let delta_e1_intersect_e1_u_e2 = self.st_intersect_expr(delta_e1, e1_u_e2);
                self.st_union(delta_e2, delta_e1_intersect_e1_u_e2)
            }
            AExpr::Top => {
                let top = self.mk_top();
                self.st_singleton(self.spp.top, top)
            }
        };

        // Cache the result
        self.delta_map.insert(state, result.clone());
        result
    }

    pub fn epsilon(&mut self, state: State) -> spp::SPP {
        if let AExpr::SPP(spp) = self.get_expr(state) { return *spp; }
        // Check if we've already calculated this
        if let Some(&spp) = self.epsilon_map.get(&state) {
            return spp;
        }

        // Clone the expression to avoid borrowing issues
        let expr = self.get_expr(state).clone();

        // Calculate epsilon for each case
        let result = match expr {
            AExpr::SPP(spp) => spp,
            AExpr::Union(states) => {
                // Pre-compute all epsilon values
                let epsilon_values: Vec<spp::SPP> =
                    states.iter().map(|&s| self.epsilon(s)).collect();

                // Then combine them
                let mut result = self.spp.zero;
                for &eps in &epsilon_values {
                    result = self.spp.union(result, eps);
                }
                result
            }
            AExpr::Intersect(states) => {
                // Pre-compute all epsilon values
                let epsilon_values: Vec<spp::SPP> =
                    states.iter().map(|&s| self.epsilon(s)).collect();

                // Then combine them
                if epsilon_values.is_empty() {
                    self.spp.one
                } else {
                    let mut result = epsilon_values[0];
                    for &eps in &epsilon_values[1..] {
                        result = self.spp.intersect(result, eps);
                    }
                    result
                }
            }
            AExpr::Xor(e1, e2) => {
                let eps1 = self.epsilon(e1);
                let eps2 = self.epsilon(e2);
                self.spp.xor(eps1, eps2)
            }
            AExpr::Difference(e1, e2) => {
                let eps1 = self.epsilon(e1);
                let eps2 = self.epsilon(e2);
                self.spp.difference(eps1, eps2)
            }
            AExpr::Complement(e) => {
                let eps = self.epsilon(e);
                self.spp.complement(eps)
            }
            AExpr::Sequence(e1, e2) => {
                let eps1 = self.epsilon(e1);
                let eps2 = self.epsilon(e2);
                self.spp.sequence(eps1, eps2)
            }
            AExpr::Star(e) => {
                let eps = self.epsilon(e);
                self.spp.star(eps)
            }
            AExpr::Dup => self.spp.zero,
            AExpr::LtlNext(_) => self.spp.zero,
            AExpr::LtlUntil(_e1, e2) => self.epsilon(e2),
            AExpr::Top => self.spp.top,
        };

        // Cache the result
        self.epsilon_map.insert(state, result);
        result
    }

    /// Returns a reference to the internal SPPstore
    pub fn spp_store(&self) -> &spp::SPPstore {
        &self.spp
    }
    
    /// Returns a mutable reference to the internal SPPstore
    pub fn spp_store_mut(&mut self) -> &mut spp::SPPstore {
        &mut self.spp
    }

    /// Checks if the given state is empty
    pub fn is_empty(&mut self, state: State) -> bool {
        self.query_epoch = self.query_epoch.wrapping_add(1);
        if self.query_epoch == 0 {
            self.seen.fill((0, self.spp.sp.zero));
            self.query_epoch = 1;
        }
        let epoch = self.query_epoch;
        let mut todo = vec![(state, self.spp.sp.one)];
        while let Some((state, incoming)) = todo.pop() {
            if self.seen.len() <= state { self.seen.resize(state + 1, (0, self.spp.sp.zero)); }
            let previous = if self.seen[state].0 == epoch { self.seen[state].1 } else { self.spp.sp.zero };
            let fresh = self.spp.sp.difference(incoming, previous);
            if self.spp.sp.is_zero(fresh) { continue; }
            self.seen[state] = (epoch, self.spp.sp.union(previous, fresh));
            let epsilon = self.epsilon(state);
            if self.spp.has_image(fresh, epsilon) { return false; }
            for (target, label) in self.delta(state).transitions {
                let next = self.spp.push(fresh, label);
                if !self.spp.sp.is_zero(next) { todo.push((target, next)); }
            }
        }
        true
    }

    /// Computes a packet transformer for the given state (equivalent to eliminating dup)
    pub fn eliminate_dup(&mut self, initial_state: State) -> spp::SPP {
        // Check cache first
        if let Some(cached_spp) = self.eliminate_dup_cache.get(&initial_state) {
            return *cached_spp;
        }

        // Phase 1: Discover all reachable states using BFS
        let mut q: VecDeque<State> = VecDeque::new();
        let mut visited_states: HashSet<State> = HashSet::new();

        q.push_back(initial_state);
        visited_states.insert(initial_state);

        let mut head = 0;
        while head < q.len() {
            let u = q[head];
            head += 1;

            for (v_state, _) in self.delta(u).get_transitions() {
                if !visited_states.contains(v_state) {
                    visited_states.insert(*v_state);
                    q.push_back(*v_state);
                }
            }
        }
        
        let all_reachable_states: Vec<State> = visited_states.into_iter().collect();

        // Phase 2: Construct initial graph for Kleene's algorithm
        // NodeRepresentation: None represents the synthetic END_NODE, Some(state) represents an original state.
        type NodeRepr = Option<State>;
        const END_NODE: NodeRepr = None;

        let mut edges: HashMap<(NodeRepr, NodeRepr), spp::SPP> = HashMap::new();

        let get_edge = |edge_map: &HashMap<(NodeRepr, NodeRepr), spp::SPP>, from_node: NodeRepr, to_node: NodeRepr, zero_spp: spp::SPP| -> spp::SPP {
            edge_map.get(&(from_node, to_node)).cloned().unwrap_or(zero_spp)
        };
        
        // Edge from synthetic start (implicit) into the initial_state, represented as END_NODE -> initial_state
        edges.insert((END_NODE, Some(initial_state)), self.spp.one);

        // Populate edges from delta and epsilon transitions
        for &u_state in &all_reachable_states {
            let u_node_repr = Some(u_state);

            // Delta transitions: u -> v
            for (v_state, spp_uv) in self.delta(u_state).get_transitions() {
                let v_node_repr = Some(*v_state);
                let current_spp = get_edge(&edges, u_node_repr, v_node_repr, self.spp.zero);
                edges.insert((u_node_repr, v_node_repr), self.spp.union(current_spp, *spp_uv));
            }

            // Epsilon transitions: u -> END_NODE
            let eps_u = self.epsilon(u_state);
            if eps_u != self.spp.zero {
                let current_spp = get_edge(&edges, u_node_repr, END_NODE, self.spp.zero);
                edges.insert((u_node_repr, END_NODE), self.spp.union(current_spp, eps_u));
            }
        }

        // Phase 3: State Elimination
        let mut nodes_for_kleene: Vec<NodeRepr> = all_reachable_states.iter().map(|&s| Some(s)).collect();
        nodes_for_kleene.push(END_NODE);


        for &k_to_eliminate_state in &all_reachable_states { // Iterate through original states to eliminate
            let k_node = Some(k_to_eliminate_state);
            
            let r_kk = get_edge(&edges, k_node, k_node, self.spp.zero);
            let r_kk_star = self.spp.star(r_kk);

            for &i_node in &nodes_for_kleene {
                if i_node == k_node { continue; }

                let r_ik = get_edge(&edges, i_node, k_node, self.spp.zero);
                if r_ik == self.spp.zero { continue; }

                for &j_node in &nodes_for_kleene {
                    if j_node == k_node { continue; } 
                    
                    let r_kj = get_edge(&edges, k_node, j_node, self.spp.zero);
                    if r_kj == self.spp.zero { continue; }

                    // Ensure mutable borrows of self.spp are clearly separated
                    let temp_seq = self.spp.sequence(r_ik, r_kk_star);
                    let path_spp = self.spp.sequence(temp_seq, r_kj);
                    
                    if path_spp != self.spp.zero {
                        let current_r_ij = get_edge(&edges, i_node, j_node, self.spp.zero);
                        let new_edge_val = self.spp.union(current_r_ij, path_spp);
                        edges.insert((i_node, j_node), new_edge_val);
                    }
                }
            }
        }
        
        // Phase 4: Result is the self-loop on END_NODE
        let result_spp = get_edge(&edges, END_NODE, END_NODE, self.spp.zero);

        // Store in cache before returning
        self.eliminate_dup_cache.insert(initial_state, result_spp);
        result_spp
    }

    fn live_packets(&mut self, initial: State) -> crate::sp::SP {
        if let Some(&live) = self.live_cache.get(&initial) { return live; }
        let mut states = vec![initial];
        let mut live = FxHashMap::default();
        live.insert(initial, self.spp.sp.zero);
        let mut predecessors: FxHashMap<State, Vec<(State, spp::SPP)>> = FxHashMap::default();
        let mut head = 0;
        while head < states.len() {
            let state = states[head];
            head += 1;
            let epsilon = self.epsilon(state);
            live.insert(state, self.spp.bwd(epsilon));
            for (target, label) in self.delta(state).transitions {
                predecessors.entry(target).or_default().push((state, label));
                if let std::collections::hash_map::Entry::Vacant(entry) = live.entry(target) {
                    entry.insert(self.spp.sp.zero);
                    states.push(target);
                }
            }
        }
        let mut todo = states;
        while let Some(target) = todo.pop() {
            if let Some(incoming) = predecessors.get(&target) {
                for &(source, label) in incoming {
                    let accepted = self.spp.pull(label, live[&target]);
                    let combined = self.spp.sp.union(live[&source], accepted);
                    if combined != live[&source] {
                        live.insert(source, combined);
                        todo.push(source);
                    }
                }
            }
        }
        let result = live[&initial];
        self.live_cache.extend(live);
        result
    }

    pub fn delta_pruned(&mut self, state: State) -> ST {
        self.live_packets(state);
        let original = self.delta(state);
        let mut transitions = HashMap::new();
        for (target, label) in original.transitions {
            let allowed_outputs = self.spp.ifwd(self.live_cache[&target]);
            let viable = self.spp.intersect(label, allowed_outputs);
            if viable != self.spp.zero { transitions.insert(target, viable); }
        }
        ST::new(transitions)
    }

    pub fn random_packet_pair(&mut self, state: State) -> Option<(Vec<bool>, Vec<bool>)> {
        let epsilon = self.epsilon(state);
        return self.spp.random_packet_pair(epsilon);
    }

    // pub fn random_packet(&self) -> Vec<bool> {
    //     let mut packet = vec![false; self.num_vars as usize];
    //     for i in 0..self.num_vars as usize {
    //         packet[i] = rand::rng().random_bool(0.5);
    //     }
    //     packet
    // }

    pub fn random_trace(&mut self, state: State, max_length: usize) -> Option<(Vec<Vec<bool>>, Option<Vec<bool>>)> {
        let mut trace = vec![];
        let live = self.live_packets(state);
        let mut current_packet = self.spp.sp.random_packet(live)?;
        let mut current_state = state;
        while trace.len() < max_length {
            trace.push(current_packet.clone());
            // Check if the current packet can be accepted by the current state
            let epsilon = self.epsilon(current_state);
            // Or transitioned from the current state
            let deltas = self.delta_pruned(current_state);
            let mut choices = Vec::new();
            if let Some(packet) = self.spp.random_output_packet_from_input(epsilon, current_packet.clone()) {
                choices.push((None, packet));
            }
            for (&target, &label) in deltas.get_transitions() {
                if let Some(packet) = self.spp.random_output_packet_from_input(label, current_packet.clone()) {
                    choices.push((Some(target), packet));
                }
            }
            assert!(!choices.is_empty(), "live residual has no accepting continuation");
            let choice = rand::random_range(0..choices.len());
            let (target, packet) = choices.swap_remove(choice);
            if let Some(target) = target {
                current_state = target;
                current_packet = packet;
            } else {
                return Some((trace, Some(packet)));
            }
        }
        // If we get here, we have a trace that is too long
        Some((trace, None))
    }

    /// Returns a string representation of the AExpr for the given state
    pub fn state_to_string(&self, state: State) -> String {
        match self.get_expr(state) {
            AExpr::SPP(spp) => format!("SPP({})", spp),
            AExpr::Union(states) => {
                let state_strings: Vec<String> =
                    states.iter().map(|&s| self.state_to_string(s)).collect();
                format!("({})", state_strings.join(" + "))
            }
            AExpr::Intersect(states) => {
                let state_strings: Vec<String> =
                    states.iter().map(|&s| self.state_to_string(s)).collect();
                format!("({})", state_strings.join(" & "))
            }
            AExpr::Xor(e1, e2) => format!(
                "({} ^ {})",
                self.state_to_string(*e1),
                self.state_to_string(*e2)
            ),
            AExpr::Difference(e1, e2) => format!(
                "({} - {})",
                self.state_to_string(*e1),
                self.state_to_string(*e2)
            ),
            AExpr::Complement(e) => format!("!{}", self.state_to_string(*e)),
            AExpr::Sequence(e1, e2) => format!(
                "({} ; {})",
                self.state_to_string(*e1),
                self.state_to_string(*e2)
            ),
            AExpr::Star(e) => format!("({})*", self.state_to_string(*e)),
            AExpr::Dup => "dup".to_string(),
            AExpr::LtlNext(e) => format!("X({})", self.state_to_string(*e)),
            AExpr::LtlUntil(e1, e2) => format!(
                "({} U {})",
                self.state_to_string(*e1),
                self.state_to_string(*e2)
            ),
            AExpr::Top => "⊤".to_string(),
        }
    }

    /// Collects all SPP indices from the expression at the given state
    pub fn collect_spps(&self, state: State, spps: &mut HashSet<spp::SPP>) {
        match self.get_expr(state) {
            AExpr::SPP(spp) => {
                spps.insert(*spp);
            }
            AExpr::Union(states) => {
                for &s in states {
                    self.collect_spps(s, spps);
                }
            }
            AExpr::Intersect(states) => {
                for &s in states {
                    self.collect_spps(s, spps);
                }
            }
            AExpr::Xor(e1, e2)
            | AExpr::Difference(e1, e2)
            | AExpr::Sequence(e1, e2)
            | AExpr::LtlUntil(e1, e2) => {
                self.collect_spps(*e1, spps);
                self.collect_spps(*e2, spps);
            }
            AExpr::Complement(e) | AExpr::Star(e) | AExpr::LtlNext(e) => {
                self.collect_spps(*e, spps);
            }
            AExpr::Dup | AExpr::Top => {}
        }
    }
}
#[cfg(test)]
mod constructor_tests {
    use super::*;

    fn relation(aut: &mut Aut, bits: u32, mask: u64) -> State {
        let m = &mut aut.spp;
        let size = 1usize << bits;
        let mut result = m.zero;
        for input in 0..size {
            for output in 0..size {
                if mask & (1 << (input * size + output)) == 0 { continue; }
                let mut atom = m.one;
                for bit in 0..bits {
                    let test = m.test(bit, input & (1 << bit) != 0);
                    atom = m.sequence(atom, test);
                }
                for bit in 0..bits {
                    let assign = m.assign(bit, output & (1 << bit) != 0);
                    atom = m.sequence(atom, assign);
                }
                result = m.union(result, atom);
            }
        }
        aut.mk_spp(result)
    }

    #[test]
    fn relation_constructors_match_general_paths_and_finite_relations() {
        for bits in 0..=2 {
            let mut aut = Aut::new(bits);
            let masks: Vec<u64> = if bits < 2 {
                (0..(1 << (1 << (2 * bits)))).collect()
            } else {
                (0..64).map(|i| (i * 104729 + 8191) & 65535).collect()
            };
            for &a in &masks {
                for &b in &masks {
                    let x = relation(&mut aut, bits, a);
                    let y = relation(&mut aut, bits, b);
                    let union = aut.mk_union(x, y);
                    assert_eq!(union, aut.mk_union_n(vec![x, y]));
                    assert_eq!(union, relation(&mut aut, bits, a | b));
                    let intersect = aut.mk_intersect(x, y);
                    assert_eq!(intersect, aut.mk_intersect_n(vec![x, y]));
                    assert_eq!(intersect, relation(&mut aut, bits, a & b));
                }
            }
        }
    }

    #[test]
    fn boolean_operations_preserve_histories() {
        let both = Expr::union(Expr::dup(), Expr::one());
        let cases = [
            (Expr::intersect(both.clone(), Expr::dup()), Expr::dup()),
            (Expr::difference(both.clone(), Expr::dup()), Expr::one()),
            (Expr::xor(both, Expr::dup()), Expr::one()),
            (Expr::intersect(Expr::complement(Expr::zero()), Expr::dup()), Expr::dup()),
            (Expr::intersect(Box::new(Expr::End), Expr::dup()), Expr::zero()),
        ];
        for (left, right) in cases {
            let mut aut = Aut::new(2);
            let difference = aut.expr_to_state(&Expr::xor(left, right));
            assert!(aut.is_empty(difference));
        }
    }
}

#[cfg(test)]
mod query_tests {
    use super::*;

    #[test]
    fn epoch_wrap_clears_seen_packets() {
        let mut aut = Aut::new(1);
        let state = aut.expr_to_state(&Expr::one());
        assert!(!aut.is_empty(state));
        aut.query_epoch = u64::MAX;
        assert!(!aut.is_empty(state));
    }
}

#[cfg(test)]
mod viability_tests {
    use super::*;

    #[test]
    fn backward_viability_matches_relation_elimination() {
        let expressions = [
            Expr::zero(),
            Expr::complement(Expr::dup()),
            Expr::union(Expr::one(), Expr::sequence(Expr::dup(), Expr::test(0, false))),
            Expr::sequence(
                Expr::star(Expr::sequence(Expr::assign(0, true), Expr::dup())),
                Expr::test(1, false),
            ),
            Expr::intersect(
                Expr::sequence(Expr::dup(), Expr::test(0, false)),
                Expr::sequence(Expr::dup(), Expr::test(1, true)),
            ),
        ];
        for expression in expressions {
            let mut aut = Aut::new(2);
            let state = aut.expr_to_state(&expression);
            let live = aut.live_packets(state);
            let relation = aut.eliminate_dup(state);
            assert_eq!(live, aut.spp.bwd(relation));
        }
    }
}

#[cfg(test)]
mod root_cache_tests {
    use super::*;

    #[test]
    fn snapshots_compare_contents_and_observe_deep_mutation() {
        let mut aut = Aut::new(2);
        let mut expr = Expr::sequence(Expr::dup(), Expr::test(0, false));
        let first = aut.expr_to_state(&expr);
        assert_eq!(first, aut.expr_to_state(&expr.clone()));
        assert_eq!(aut.root_snapshots.len(), 1);
        if let Expr::Sequence(_, child) = expr.as_mut() { **child = Expr::Test(0, true); }
        let second = aut.expr_to_state(&expr);
        assert_ne!(first, second);
        assert_eq!(second, aut.compile_to_state(&expr));
        let mut other = Aut::new(2);
        let state = other.expr_to_state(&expr);
        assert_eq!(state, other.compile_to_state(&expr));
        assert_eq!(other.root_snapshots.len(), 1);
    }

    #[test]
    fn snapshots_preserve_history_and_eviction_only_recompiles() {
        let mut aut = Aut::new(2);
        let first = aut.expr_to_state(&Expr::One);
        let dup = aut.expr_to_state(&Expr::Dup);
        assert_ne!(first, dup);
        let mut expr = Expr::dup();
        for _ in 0..20 {
            expr = Expr::sequence(expr, Expr::dup());
            let cached = aut.expr_to_state(&expr);
            assert_eq!(cached, aut.compile_to_state(&expr));
        }
        assert_eq!(aut.root_snapshots.len(), 16);
        assert_eq!(first, aut.expr_to_state(&Expr::One));
    }

    #[test]
    fn encoding_is_bounded_and_invalid_branches_are_not_cached() {
        let mut words = vec![0; 65536];
        assert!(!Aut::encode_core(&Expr::One, &mut words));
        assert_eq!(words.len(), 65536);
        let mut aut = Aut::new(1);
        aut.expr_to_state(&Expr::One);
        let invalid = Expr::union(Expr::one(), Box::new(Expr::Var("missing".into())));
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| aut.expr_to_state(&invalid)));
        assert!(result.is_err());
        assert_eq!(aut.root_snapshots.len(), 1);
    }
}
