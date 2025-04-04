use crate::spp;

// An AExpr represents an automaton state.
// This is essentially a compressed and hash-consed form of a NetKAT expression.
enum AExpr {
    SPP(spp::SPP), // We keep field tests and mutations and combinations thereof in SPP form
    Union(AExp, AExp),      // e1 + e2
    Intersect(AExp, AExp),  // e1 & e2
    Xor(AExp, AExp),        // e1 ^ e2
    Difference(AExp, AExp), // e1 - e2
    Complement(AExp),      // !e1
    Sequence(AExp, AExp),   // e1; e2
    Star(AExp),            // e*
    Dup,                  // dup
    LtlNext(AExp),         // X e
    LtlUntil(AExp, AExp),   // e1 U e2
}

type AExp = Hc<AExpr>;

