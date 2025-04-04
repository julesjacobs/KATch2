# KATch-LTL

A reimplementation of KATch that supports LTL queries. Fundamental changes in the internals are necessary to support LTL.

## Project Structure

The project consists of several key components:

- `src/expr.rs`: NetKAT expressions
- `src/parser.rs`: NetKAT expression parser
- `src/sp.rs`: Symbolic packet data structure
  - Represents a set of packets
  - Operations: zero, one, union, intersect, complement, ifelse, test
- `src/spp.rs`: Symbolic packet program data structure
  - Represents a relation between packets
  - Operations: zero, one, top, union, intersect, complement, sequence, star, reverse, ifelse, test, assign
  - Note: May need additional operations like forward, backward
- `src/st.rs`: Symbolic transitions
- `src/aut.rs`: Symbolic NetKAT automata
- `src/expr_to_aut.rs`: Converts expressions to automata using derivatives
- `src/elim.rs`: Performs dup elimination on automata, converting to spp using Kleene's algorithm
- `src/prune.rs`: Prunes NetKAT automata through forward-backward analysis
- `src/main.rs`: Command line interface

## SPs and SPPs

- SP: represents symbolic packets (sets of concrete packets, represented as a BDD)
- SPP: represents symbolic packet programs (relations on concrete packets, represented as a BDD)

Our BDDs always store all intermediate levels. This is particularly relevant for SPP, where it is not clear what a missing level would indicate (zero, one, or top for the missing variables). In the future, we can investigate whether it is profitable do introduce a more complex scheme that can skip intermediate levels.

**Difference with KATch:** Unlike KATch, we have only binary fields, thus significantly simplifying the implementation of SPs and SPPs. Additionally, we support complement on SPPs, which KATch does not support (it would be possible to support in KATch, but it would require significant re-engineering of SPPs, due to the unbounded domain).

## STs

- ST<T>: symbolic transition

Symbolic transitions represent, for each T, a set of packet pairs that can transition to T. These are represented as a finite map from T to SPP's. 

A symbolic transition can be deterministic or nondeterministic, depending on whether the SPPs associated with different T's are disjoint. We typically keep ST's in deterministic form.

## Aut

Automata are unlabeled nodes connected via SPPs. Since each SPP represents packet pairs (pk1, pk2), the language of an Aut is a string of such packet pairs. However, since this represents a packet transformation from pk1 to pk2, the n-th out packet must be the same as the (n+1)-th in packet. That is, in a string ... (in_i, out_i) (in_{i+1}, out_{i+1}) ... we must have out_i = in_{i+1}. Strings that violate this principle are not considered to be part of the language accepted by the Aut.

### Accepting states

To represent acceptance, we have two options:

- Do it like KATch, and have a special output transition on each node that accepts a string after a final packet transformation.
- Mark each state as either accepting or non-accepting, similar to DFAs/NFAs.

The latter may be more attractive, as is more uniform: there is no need to treat the last character in the string different from the rest.

## Syntax

The language supports the following expressions:

```
e ::= 
    | 0           -- zero, drop packet
    | 1           -- one, forward packet
    | T           -- top, turns any packet into any other
    | field := value  -- field assignment
    | field == value  -- field test
    | e1 + e2     -- union, nondeterminism
    | e1 & e2     -- intersection
    | e1 ^ e2     -- xor
    | e1 - e2     -- difference
    | !e1         -- complement, negation
    | e1; e2      -- sequence
    | e*          -- star, iteration
    | dup         -- log current packet to trace
    | X e         -- LTL next
    | e1 U e2     -- LTL until (maybe change this into LDL)

field ::= x0 | x1 | x2 | ... | xk  -- packet forms a bitfield
value ::= 0 | 1
```

Note: The parser takes `k` as an argument to determine the number of available fields.

