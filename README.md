# KATch2

A next-gen version of KATch that:
- Is written in Rust
- Supports a reversible version of NetKAT
- Supports negation
- Will support LTL queries

For now, KATch2 only supports binary fields.

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

## Web UI

KATch2 includes a self-contained web UI for interactive NetKAT analysis. See `ui/` directory:

- **Deploy**: Copy `ui/katch2ui/` to your website
- **Use**: `<script type="module" src="katch2ui/katch2-editor.js"></script>`
- **Write**: `<netkat>x0 := 0; x1 := 1</netkat>`

For complete documentation, see [`ui/README.md`](ui/README.md).

## Future

Immediate TODOs:
1. Improve the UI
2. Implement pruning
3. Implement a parser for the katch1 fuzz tests (https://github.com/cornell-netlab/KATch/blob/master/nkpl/tests/fuzz100k.nkpl)
4. Better comment the code & improve the code in general
5. Add info to the UI about the syntax, what the SPP figures mean, what the automaton states/transitions/epsilons mean.

### Later

Other interesting operations to support:
1. The Phi function that removes dups (which is no-trivial in combination with the extended boolean operators, since you can't just push Phi inside them)
2. A projection operator that removes certain packet fields from traces. For example, if you are only interested in the paths, you could project out only the switch field and remove other fields
3. An un-projection operator that materializes the automaton state as an explicit field in the packet, representing a netkat program as a single SPP.
4. Pruning a netkat automaton to distill the SPPs to the minimum, removing "dead" elements of SPPs, such that you have the property that any packet pair in any internal SPP always appears in some guarded string of the entire automaton. This generalizes checking whether an automaton is empty (a semantically empty automaton would distill down to nothing)
5. LTL/stackat/probabilities/transducers/etc
