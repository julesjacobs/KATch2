// Symbolic transitions ST<T>
// Symbolic transitions represent, for each T, a set of packet pairs that can transition to T. These are represented as a finite map from T to SPP's. 
// A symbolic transition can be deterministic or nondeterministic, depending on whether the SPPs associated with different T's are disjoint. We typically keep ST's in deterministic form.

use std::collections::HashMap;
use std::hash::Hash;
use crate::spp;

pub struct ST<T>(HashMap<T, spp::SPP>);

impl<T: Hash> ST<T> {
    pub fn op1<R>(&self, op: fn(T) -> R) -> ST<R> {
        todo!();
    }
    pub fn op2<R>(&self, other: &ST<T>, op: fn(T, T) -> R) -> ST<R>{
        todo!();
    }
}