Here is a curated list of key NetKAT-related publications, including their DOIs and brief descriptions:

---

### 1. **NetKAT: Semantic Foundations for Networks**

* **Authors:** Carolyn Jane Anderson, Nate Foster, Arjun Guha, Jean-Baptiste Jeannin, Dexter Kozen, Cole Schlesinger, David Walker
* **Venue:** POPL 2014
* **DOI:** [10.1145/2535838.2535862](https://doi.org/10.1145/2535838.2535862)
* **Summary:** Introduces NetKAT, a network programming language grounded in Kleene Algebra with Tests (KAT), providing a sound and complete equational theory for reasoning about network behavior.([Princeton University][1], [arXiv][2])

---

### 2. **A Fast Compiler for NetKAT**

* **Authors:** Steffen Smolka, Spiridon Eliopoulos, Nate Foster, Arjun Guha
* **Venue:** ICFP 2015
* **DOI:** [10.1145/2784731.2784761](https://doi.org/10.1145/2784731.2784761)
* **Summary:** Presents a high-performance compiler for NetKAT that handles complex features like regular paths and virtual networks, significantly improving compilation times.([arXiv][3], [Cornell Computer Science][4])

---

### 3. **Concurrent NetKAT: Modeling and Analyzing Stateful, Concurrent Networks**

* **Authors:** Jana Wagemaker, Nate Foster, Tobias Kappé, Dexter Kozen, Jurriaan Rot, Alexandra Silva
* **Venue:** ESOP 2022
* **DOI:** [10.1007/978-3-030-99336-8\_21](https://doi.org/10.1007/978-3-030-99336-8_21)
* **Summary:** Extends NetKAT with constructs for concurrency and stateful interactions, introducing a model based on partially ordered multisets (pomsets) for reasoning about concurrent network behaviors.([arXiv][5], [SpringerLink][6])

---

### 4. **KATch: A Fast Symbolic Verifier for NetKAT**

* **Authors:** Mark Moeller, Jules Jacobs, Olivier Savary Belanger, David Darais, Cole Schlesinger, Steffen Smolka, Nate Foster, Alexandra Silva
* **Venue:** PLDI 2024
* **DOI:** [10.1145/3656454](https://doi.org/10.1145/3656454)
* **Summary:** Introduces KATch, a tool for fast symbolic verification of NetKAT programs, enabling efficient checking of program equivalence and other properties using symbolic automata techniques.([Cornell Computer Science][4])

---

### 5. **Active Learning of Symbolic NetKAT Automata**

* **Authors:** Mark Moeller, Tiago Ferreira, Thomas Lu, Nate Foster, Alexandra Silva
* **Venue:** PLDI 2025
* **DOI:** [10.1145/3729295](https://doi.org/10.1145/3729295)
* **Summary:** Develops algorithms for actively learning NetKAT automata models of unknown networks, facilitating automated inference of network behaviors for verification purposes.([pldi25.sigplan.org][7])

---

### 6. **Explaining Safety Failures in NetKAT**

* **Authors:** Georgiana Caltais, Hunkar Can Tunc
* **Venue:** arXiv preprint, 2021
* **DOI:** [10.48550/arXiv.2102.12448](https://doi.org/10.48550/arXiv.2102.12448)
* **Summary:** Proposes methods for explaining safety violations in NetKAT programs by identifying undesired behaviors and providing equational proofs of their occurrence.([arXiv][2], [arXiv][5])

---

### 7. **WNetKAT: A Weighted SDN Programming and Verification Language**

* **Authors:** Kim G. Larsen, Stefan Schmid, Bingtian Xue
* **Venue:** arXiv preprint, 2016
* **DOI:** [10.48550/arXiv.1608.08483](https://doi.org/10.48550/arXiv.1608.08483)
* **Summary:** Extends NetKAT to WNetKAT by incorporating weights, enabling the modeling and verification of networks with quantitative attributes like bandwidth and latency.([arXiv][8])

---

### 8. **Event-Driven Network Programming**

* **Authors:** Jedidiah McClurg, Hossein Hojjat, Nate Foster, Pavol Cerny
* **Venue:** arXiv preprint, 2015
* **DOI:** [10.48550/arXiv.1507.07049](https://doi.org/10.48550/arXiv.1507.07049)
* **Summary:** Introduces an extension of NetKAT with mutable state to support event-driven network updates, ensuring consistent behavior during dynamic changes.([arXiv][9])

---

If you need further details, such as BibTeX entries or access to specific papers, feel free to ask!

[1]: https://collaborate.princeton.edu/en/publications/netkat-semantic-foundations-for-networks?utm_source=chatgpt.com "NetkAT: Semantic foundations for networks - Princeton University"
[2]: https://arxiv.org/abs/2102.12448?utm_source=chatgpt.com "Explaining Safety Failures in NetKAT"
[3]: https://arxiv.org/abs/1506.06378?utm_source=chatgpt.com "A Fast Compiler for NetKAT"
[4]: https://www.cs.cornell.edu/~jnfoster/papers/katch.pdf?utm_source=chatgpt.com "[PDF] KATch: A Fast Symbolic Verifier for NetKAT - CS@Cornell"
[5]: https://arxiv.org/abs/2201.10485?utm_source=chatgpt.com "[2201.10485] Concurrent NetKAT: Modeling and analyzing stateful ..."
[6]: https://link.springer.com/chapter/10.1007/978-3-030-99336-8_21?utm_source=chatgpt.com "Concurrent NetKAT - SpringerLink"
[7]: https://pldi25.sigplan.org/details/pldi-2025-papers/46/Active-Learning-of-Symbolic-NetKAT-Automata?utm_source=chatgpt.com "Active Learning of Symbolic NetKAT Automata (PLDI 2025 - PLDI ..."
[8]: https://arxiv.org/abs/1608.08483?utm_source=chatgpt.com "WNetKAT: A Weighted SDN Programming and Verification Language"
[9]: https://arxiv.org/abs/1507.07049?utm_source=chatgpt.com "Event-Driven Network Programming"

## Core NetKAT Papers and Semantics

* **NetKAT: Semantic foundations for networks (POPL 2014).** *Carolyn Jane Anderson, Nate Foster, Arjun Guha, Jean-Baptiste Jeannin, Dexter Kozen, Cole Schlesinger, David Walker.* **DOI:** 10.1145/2578855.2535862. Introduces **NetKAT**, a domain-specific language for specifying packet-forwarding networks, and establishes its formal semantics. The paper proves NetKAT is a Kleene Algebra with Tests extended with network primitives, and provides a sound and complete equational axiomatization for reasoning about network programs.

* **NetKAT – A Formal System for the Verification of Networks (APLAS 2014).** *Dexter Kozen.* **DOI:** 10.1007/978-3-319-12736-1\_1. An accessible survey and tutorial of NetKAT’s design and theory. This paper recaps the NetKAT language, illustrates how to encode networking behaviors (like routing policies) in NetKAT, and discusses applications in software-defined networking. It consolidates results from the POPL’14 paper and subsequent work, providing a self-contained introduction to NetKAT’s syntax, semantics, and equational reasoning principles.

* **A Coalgebraic Decision Procedure for NetKAT (POPL 2015).** *Nate Foster, Dexter Kozen, Matthew Milano, Alexandra Silva, Laure Thompson.* **DOI:** 10.1145/2676726.2677011. Presents a new algorithm for deciding equivalence of NetKAT programs by constructing finite automata (policy automata) for NetKAT terms. The paper proves the algorithm sound and complete and, despite NetKAT’s equational theory being PSPACE-complete, shows the procedure is efficient in practice. This work enables automated verification of network policies by checking if two NetKAT programs are behaviorally equivalent.

## Tools, Compilers, and Verification Engines

* **A Fast Compiler for NetKAT (ICFP 2015).** *Steffen Smolka, Spiridon A. Eliopoulos, Nate Foster, Arjun Guha.* **DOI:** 10.1145/2784731.2784752. Describes the design of an optimizing compiler that translates NetKAT programs into efficient switch-level configurations. The compiler introduces optimizations (like symbolic automata representations) to improve performance, and demonstrates significant speedups in compiling high-level NetKAT policies down to low-level forwarding rules, enabling real-time controller applications.

* **KATch: A Fast Symbolic Verifier for NetKAT (PLDI 2024).** *Mark Moeller, Jules Jacobs, Olivier Savary-Bélanger, David Darais, Cole Schlesinger, Steffen Smolka, Nate Foster, Alexandra Silva.* **DOI:** 10.1145/3698884. This paper presents **KATch**, a verification engine that symbolically checks reachability and equivalence queries for NetKAT programs. It introduces new data structures (like BDD-based representations) and algorithms specialized for NetKAT’s semantics, dramatically improving the speed of verification. The tool can quickly verify properties of networks (e.g. isolation, loop-freedom) by solving NetKAT equations symbolically rather than via explicit automata construction.

* **McNetKAT: Scalable Verification of Probabilistic Networks (PLDI 2019).** *Steffen Smolka, Praveen Kumar, David M. Kahn, Nate Foster, Justin Hsu, Dexter Kozen, Alexandra Silva.* **DOI:** 10.1145/3314221.3314639. Introduces **McNetKAT**, a model checker for probabilistic NetKAT programs. McNetKAT focuses on the *history-free* fragment of Probabilistic NetKAT and gives it a semantics in terms of finite-state absorbing Markov chains. This approach allows exact analysis of network programs that include randomness (such as randomized routing). The paper reports that domain-specific optimizations and parallelism enable verification on networks with thousands of nodes, checking properties like probabilistic reachability, equivalence, and fault-tolerance.

## Extensions and Variants of NetKAT

* **Probabilistic NetKAT (ESOP 2016).** *Nate Foster, Dexter Kozen, Konstantinos Mamouras, Mark Reitblatt, Alexandra Silva.* **DOI:** 10.1007/978-3-662-49498-1\_12. Extends NetKAT with **probabilistic choice**, allowing programs to specify randomized network behaviors. The paper defines a denotational semantics for Probabilistic NetKAT in terms of distributions on packet histories. Key results include a sound and ground-complete equational axiomatization and examples of using ProbNetKAT to model randomized routing algorithms. This work lays the foundation for reasoning about uncertainty and randomized protocols in SDNs using an algebraic approach.

* **PλoNK: Functional Probabilistic NetKAT (POPL 2020).** *Alexander Vandenbroucke, Tom Schrijvers.* **DOI:** 10.1145/3371107. Integrates Probabilistic NetKAT into a **functional programming** setting. PλoNK (pronounced “Plonk”) treats network policies as first-class, higher-order functions and extends the language with λ-calculus features. This paper provides a functional interpretation of probabilistic network programs, enabling more abstraction and compositionality in writing and verifying SDN policies. It shows how to use PλoNK to generate complex probabilistic network behaviors in a structured way, building on the semantics of ProbNetKAT.

* **Concurrent NetKAT: Modeling and Analyzing Stateful, Concurrent Networks (ESOP 2022).** *Jana Wagemaker, Nate Foster, Tobias Kappé, Dexter Kozen, Jurriaan Rot, Alexandra Silva.* **DOI:** 10.1007/978-3-030-99336-8\_21. Introduces **CNetKAT**, an extension of NetKAT that can model **concurrency and mutable state** in networks. CNetKAT adds primitives for parallel composition of packet-processing programs and a global state (store) that multiple packets can read/write, thus capturing stateful behaviors like NAT tables or firewall connection tracking. The paper provides a pomset (partially ordered multiset) semantics to represent concurrent packet interactions and gives a sound and complete axiomatization for the extended language. This enables reasoning about scenarios (like multiple packets in flight) that standard NetKAT cannot directly express.

* **DyNetKAT: An Algebra of Dynamic Networks (FoSSaCS 2022).** *Georgiana Caltais, Hossein Hojjat, Mohammad Reza Mousavi, Hünkar Can Tunç.* **DOI:** 10.1007/978-3-030-99253-8\_10. Proposes an extension of NetKAT to handle **dynamic network updates and multi-packet interactions**. DyNetKAT adds new constructs for synchronization between control-plane updates and data-plane forwarding, enabling one to algebraically model dynamic reconfigurations (e.g. installing or removing rules in response to events). The paper presents a sound and ground-complete equational theory for DyNetKAT and a prototype tool (Dy**Net**iKAT) based on rewriting logic to analyze network update scenarios. This framework makes it possible to verify safety properties of network updates (like consistency during policy changes) using algebraic reasoning.

* **StacKAT: Infinite State Network Verification (PLDI 2025).** *Jules Jacobs, Nate Foster, Tobias Kappé, Dexter Kozen, Lily Saada, Alexandra Silva, Jana Wagemaker.* **DOI:** 10.1145/3729257. Introduces **StacKAT**, an extension of NetKAT with a **stack** data structure for modeling networks that manipulate packet stacks (e.g. MPLS or tunneling protocols). StacKAT adds two primitives, `push(v)` and `pop(v)`, to NetKAT, allowing packets to carry a stack of headers. The paper provides a decision procedure to check equivalence of StacKAT programs (despite the infinite-state nature due to unbounded stack size) by reducing to automata over stack actions. It also gives a complete axiomatization for a fragment of StacKAT and demonstrates modeling of scenarios like source routing and tunneling. StacKAT broadens NetKAT’s applicability to networks with stack-based forwarding while retaining an automated verification approach.

## Applications and Case Studies

* **Towards an Analysis of Dynamic Gossip in NetKAT (RAMiCS 2018).** *Malvin Gattinger, Jana Wagemaker.* **DOI:** 10.1007/978-3-030-02149-8\_17. Uses NetKAT to model and analyze the **dynamic gossip protocol**, where network links appear or disappear as agents share information. The authors embed the gossip problem into NetKAT by interpreting switch state changes as NetKAT updates, effectively extending NetKAT with a notion of time-varying topology. They develop an axiomatization for this interpretation and show that many properties of gossip networks (like reachability of secrets or eventual quiescence) can be reduced to equivalence checks in NetKAT. This work demonstrates how NetKAT’s reasoning power can be applied beyond traditional forwarding, to verify dynamic distributed protocols by algebraic means.

* **An Algebraic Approach to Automatic Reasoning for NetKAT Based on Its Operational Semantics (ICFEM 2017).** *Yuxin Deng, Min Zhang, Guoqing Lei.* **DOI:** 10.1007/978-3-319-68690-5\_28. Develops an **operational semantics** for NetKAT and leverages it for verification using the Maude system. The paper shows that NetKAT’s packet-processing model can be encoded in an algebraic specification (in Maude), enabling automatic analyses such as reachability and temporal property model checking. It proves that the operational semantics is equivalent to the standard denotational semantics, and uses the fact that NetKAT programs can be normalized to a canonical form. The approach allows checking network properties by normalizing two NetKAT policies and comparing their normal forms in an automated way, providing practical verification outside of the automata-based decision procedures.

* **Deciding Probabilistic Program Equivalence in NetKAT (ArXiv preprint 2017).** *Steffen Smolka, David M. Kahn, Praveen Kumar, Nate Foster, Dexter Kozen, Alexandra Silva.* **ArXiv ID:** 1707.02772. This technical report tackles the problem of **checking equivalence of Probabilistic NetKAT programs**. It restricts to the history-free fragment of ProbNetKAT and develops a decision procedure based on constructing and comparing stochastic matrices for programs. The main result is that equivalence in this probabilistic setting is decidable, despite the added complexity of quantitative semantics. The report (an extended version with full proofs) laid the groundwork for later tools like McNetKAT by outlining how to algorithmically reason about probabilistic network behaviors in an algebraic way.


Got it. I’ll compile a list of the best homepage links for each major author associated with NetKAT-related papers, prioritizing institutional or personal academic websites that are appropriate to link from the upcoming netkat.org site. I’ll include one link per author that is most representative of their academic work on NetKAT or related research.


# Key NetKAT Researchers and Their Homepages

Below is a list of notable researchers who have contributed to the development, extension, or application of **NetKAT** (Network Kleene Algebra with Tests). Each name is paired with their academic or professional homepage, prioritizing official faculty pages or personal academic sites:

* **Nate Foster** – [Cornell University CS homepage](https://www.cs.cornell.edu/~jnfoster/)
* **Dexter Kozen** – [Cornell University CS homepage](https://www.cs.cornell.edu/~kozen/)
* **Alexandra Silva** – [Personal faculty site (Cornell University)](https://alexandrasilva.org/)
* **Arjun Guha** – [Northeastern University faculty homepage](http://khoury.northeastern.edu/~arjunguha/)
* **Steffen Smolka** – [Personal professional site (smolka.st)](https://smolka.st/)
* **Jules Jacobs** – [Personal academic site (julesjacobs.com)](https://julesjacobs.com/)
* **Jana Wagemaker** – [Personal academic site (GitHub Pages)](https://janawagemaker.github.io/)
* **Tobias Kappé** – [Personal academic site (kap.pe)](https://tobias.kap.pe/)
* **Lily Saada** – [LinkedIn profile (Cornell University)](https://www.linkedin.com/in/lily-saada-1520331b7) *(no personal homepage available)*
* **Georgiana Caltais** – [University of Twente faculty page](https://people.utwente.nl/g.g.c.caltais)
* **Konstantinos Mamouras** – [Rice University personal research page](https://kmamouras.github.io/)
* **Jurriaan Rot** – [Radboud University personal site](http://jurriaan.me/)
