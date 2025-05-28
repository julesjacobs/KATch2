Great! I’ll compile a list of NetKAT-related software projects, including tools like Frenetic, Katch (both versions), and StacKAT. Each entry will include author names, a short description, GitHub or source links if available, and relevant details such as language, scope, and maintenance status if known.


# NetKAT-Related Software Projects

Below is a comprehensive list of software projects related to **NetKAT** (Network Kleene Algebra with Tests). Each project entry includes the project name, key authors or developers, a description of its purpose or NetKAT features, implementation language(s), source repository if available, and current status.

## Frenetic (NetKAT Controller Framework)

* **Authors/Core Developers:** Nate Foster, Arjun Guha, Joshua Reich, Mark Reitblatt, Rob Harrison, Cole Schlesinger, and others from the Frenetic project.
* **Description:** Frenetic is a high-level SDN programming language and controller runtime. It originally featured the NetCore language and later adopted **NetKAT as its policy language** for writing network applications. Frenetic provides abstractions for composing network forwarding policies and includes a runtime system for installing these policies on OpenFlow switches.
* **Language:** OCaml (with some Python components).
* **Repository:** [frenetic-lang/frenetic](https://github.com/frenetic-lang/frenetic) on GitHub.
* **Status:** Largely a research project and teaching tool; active development has slowed. Latest major version 5.x integrated NetKAT and was released around 2018–2019. The project is now **archived/maintenance mode** (no recent commits) as research focus has shifted to newer NetKAT tools.

## NetKAT Automata (Fast NetKAT Compiler)

* **Authors/Core Developers:** Steffen Smolka, Spiridon Eliopoulos, Nate Foster, Arjun Guha.
* **Description:** Sometimes referred to as the “*Fast Compiler for NetKAT*,” this project developed a high-performance compiler and decision procedure for NetKAT programs. It introduced an optimized intermediate representation based on binary decision diagrams (BDDs) and *policy automata* to compile NetKAT policies two orders of magnitude faster than previous systems. The compiler handles complex features like regular path expressions and virtual network constructs, automatically inserting tags for end-to-end path isolation.
* **Language:** OCaml (with libraries for BDDs and automata).
* **Repository:** [frenetic-lang/netkat-automata](https://github.com/frenetic-lang/netkat-automata) on GitHub.
* **Status:** **Research prototype** (circa 2015). Provided as an artifact for ICFP 2015. Not under active development; its ideas have influenced subsequent tools and were integrated into other NetKAT compilers.

## Probabilistic NetKAT (ProbNetKAT)

* **Authors/Core Developers:** Nate Foster, Dexter Kozen, Konstantinos Mamouras, Mark Reitblatt, Alexandra Silva.
* **Description:** ProbNetKAT is an extension of NetKAT that adds **probabilistic choice** to the language. This allows network programs to specify randomized behaviors (for example, randomly load-balancing traffic). The project provided a formal denotational semantics in terms of distributions over packet histories, along with a sound and ground-complete equational axiomatization for reasoning about probabilistic network programs.
* **Language:** The semantics and proofs were developed formally (e.g. Coq and math), and prototype implementations have appeared in OCaml/Coq. (ProbNetKAT’s ideas are also embedded in later systems like McNetKAT and PλᴏNK.)
* **Repository:** No standalone public code repository for the original 2016 language (it was a theoretical development), but see McNetKAT and PλᴏNK for implementations.
* **Status:** **Research extension** of NetKAT (ESOP 2016). The language is used in academic contexts. It is not an active software project on its own, but its concepts live on in tools like McNetKAT and follow-up work (e.g. the functional PλᴏNK interpreter).

## McNetKAT (Model Checker for ProbNetKAT)

* **Authors/Core Developers:** Steffen Smolka, Praveen Kumar, David M. Kahn, Nate Foster, Justin Hsu, Dexter Kozen, Alexandra Silva.
* **Description:** McNetKAT is a **model checker for probabilistic NetKAT** programs. It focuses on the *history-free fragment* of ProbNetKAT, using finite-state absorbing Markov chains to exactly analyze probabilistic network behaviors. McNetKAT introduced a new semantics for NetKAT’s probabilistic guards to enable scalable verification of properties like reachability with random forwarding decisions. It includes optimizations (like parallelizing across cores) to handle networks with thousands of nodes.
* **Language:** OCaml (built on the Frenetic/NetKAT codebase) with parallel C/C++ extensions.
* **Repository:** Distributed as an artifact via the Frenetic project (e.g., a special branch/release *mcnetkat-pldi19* in frenetic-lang/frenetic). No separate public repo named “McNetKAT” exists, but instructions and a Vagrant VM were provided for replication.
* **Status:** **Research tool** (PLDI 2019). Provided as an experimental verifier for probabilistic networks. Not in active development since 2019, but it achieved its research goals and can be used for academic purposes (with the provided VM or source snapshot).

## Temporal NetKAT (Temporal Logic Extensions)

* **Authors/Core Developers:** Ryan Beckett, Michael Greenberg, David Walker.
* **Description:** Temporal NetKAT extends NetKAT with temporal logic operators, specifically constructs from past-time linear temporal logic (LTL) to reason about packet histories. This allows policies that depend on the sequence of events a packet has seen (e.g., “if a packet visited firewall X earlier, then ...”). The project created a **compiler for Temporal NetKAT** that translates temporal NetKAT policies into automata, intersecting them with network models to check properties over traces. It can answer reachability queries that involve historical conditions.
* **Language:** OCaml (compiler and prototype interpreter), plus some Python for evaluation scripts.
* **Repository:** [rabeckett/Temporal-NetKAT](https://github.com/rabeckett/Temporal-NetKAT) on GitHub – contains the PLDI 2016 artifact (compiler source and experiments).
* **Status:** **Research prototype** (PLDI 2016). Not actively maintained after 2016. Served as a proof-of-concept for adding LTLf-based queries to NetKAT. The ideas influence subsequent verification frameworks, but the specific codebase is archived.

## MatchKAT (Match-Action Algebra)

* **Authors/Core Developers:** Xiang Long (with guidance from Georgiana Caltais, Hünkar Can Tunç).
* **Description:** MatchKAT is an algebraic framework inspired by NetKAT, tailored for modeling **match-action processing** in switches. It provides a language to reason about sequences of match-action rules (like those in OpenFlow pipelines) in an algebraic way. While NetKAT treats packet processing abstractly, MatchKAT’s primitives correspond directly to match conditions and actions, making it convenient for analyzing switch configurations and pipeline equivalences.
* **Language:** The work is primarily theoretical. A prototype could be implemented in a proof assistant or Python, but no widely known implementation exists yet (the arXiv preprint defines the algebra and semantics).
* **Repository:** No public code repository (as of 2021 it’s an academic proposal). Possibly an internal prototype or Coq formalization was used for proofs.
* **Status:** **Research proposal** (arXiv 2021). Not a software tool per se; rather, a foundational theory. It could inform future NetKAT-based compilers or verification tools that deal with match-action pipelines.

## KATch (NetKAT Verifier, Scala)

* **Authors/Core Developers:** Mark Moeller, Jules Jacobs, Olivier Savary-Belanger, David Darais, Cole Schlesinger, Steffen Smolka, Nate Foster, Alexandra Silva.
* **Description:** KATch is a **fast symbolic verifier for NetKAT** programs. It implements an algorithm to check *equivalence and refinement* between two NetKAT policies or to verify reachability properties by solving NetKAT inclusion queries. KATch introduces NKPL (NetKAT Property Language), a domain-specific language to pose verification queries. Internally, it represents NetKAT programs as symbolic automata and uses optimizations to quickly decide if two programs are behaviorally equivalent. It can also produce counterexample packet histories when programs differ.
* **Language:** Scala (runs on the JVM). The implementation uses Scala for the core engine and integrates with tools like Graphviz for visualization.
* **Repository:** [julesjacobs/KATch](https://github.com/julesjacobs/KATch) on GitHub (PLDI 2024 artifact) – includes source, Docker images, and a tutorial for NKPL.
* **Status:** **Research/experimental tool** (PLDI 2024). Active in the research sense – developed for a 2024 paper – but not an end-user product. It is available via Docker and can be used to reproduce the authors’ experiments or verify custom policies. Future development has shifted to the Rust-based successor, KATch2.

## KATch2 (Next-Gen NetKAT Verifier, Rust)

* **Authors/Core Developers:** Jules Jacobs, Mark Moeller (and the Cornell/ULondon NetKAT team).
* **Description:** KATch2 is the **next-generation version of the KATch verifier**, redesigned for higher performance and expanded features. It is written in Rust for efficiency and offers advanced capabilities: it supports a *reversible* form of NetKAT (allowing reasoning about packet histories backwards), includes *negation* of predicates, and is planned to support LTL (temporal) queries natively. KATch2 also comes with a web-based **NetKAT Playground** UI for interactive analysis of NetKAT programs.
* **Language:** Rust (for the core verification engine and decision procedure). The project also contains a WebAssembly/TypeScript front-end for the browser-based editor.
* **Repository:** [julesjacobs/KATch2](https://github.com/julesjacobs/KATch2) on GitHub – open source (early development, \~200+ commits).
* **Status:** **Active research development** (as of 2024–2025). KATch2 is under active development by the NetKAT research community. It is usable in prototype form (the GitHub README provides instructions and a browser UI), and is likely to be the basis of ongoing NetKAT verification research.

## StacKAT (NetKAT with Stack Extensions)

* **Authors/Core Developers:** Jules Jacobs, Nate Foster, Tobias Kappé, Dexter Kozen, Lily Saada, Alexandra Silva, Jana Wagemaker.
* **Description:** StacKAT is an extension of NetKAT that introduces a **stack data structure** into the language for modeling networks that manipulate packet payloads or tunneling headers. In StacKAT, programs can push to or pop from a stack attached to each packet, in addition to the standard NetKAT header assignments and tests. This enables modeling behaviors like packet parsing, encapsulation (e.g. MPLS or VXLAN), and in-network telemetry that were impossible in vanilla NetKAT. The StacKAT project also provides a decision procedure for program equivalence (an automata-based verifier) despite the language’s infinite-state nature (due to the unbounded stack). The verifier can check if two StacKAT programs are equivalent and produce counterexample traces when they differ (a web “Playground” demo is available).
* **Language:** Prototype implemented in Scala (built on KATch) or Rust – the paper’s artifact includes a web-based playground (likely using a compiled WASM verifier). The core decision procedure is based on automata theory and was evaluated as a research prototype.
* **Repository:** No public repo yet (as of its PLDI 2025 publication) beyond the artifact. A web demo (playground) is hosted for trying out StacKAT equivalence queries.
* **Status:** **Research prototype** (PLDI 2025). This is cutting-edge work extending NetKAT’s theoretical limits. The tool is at a proof-of-concept stage and is being used in ongoing research. It is not production-ready, but it demonstrates new capabilities (stack operations in policies) and will likely evolve through further academic development.

## Google’s NetKAT (C++ Implementation)

* **Authors/Core Developers:** Google Open Source Networking Team (exact contributors not public; labeled as “The NetKAT authors” in code).
* **Description:** This is a **C++ implementation of NetKAT** developed within Google. It provides a domain-specific language and system for specifying and verifying network behavior, aiming to bring NetKAT’s formal power to industrial use. Key features include automated reasoning about policies, modular composition of network behaviors, and a strong mathematical foundation (in line with academic NetKAT). The implementation is expected to diverge slightly from the academic definition as Google optimizes it for real-world performance and scalability. The project emphasizes a clean API for network analysis and may integrate with Google’s networking toolchain (e.g., for policy verification or synthesis in large networks).
* **Language:** C++ (with Bazel build). A small portion uses Starlark for build/test configuration.
* **Repository:** [google/netkat](https://github.com/google/netkat) on GitHub – open-sourced in 2024. (Note: This is an **unofficial** Google project, not an officially supported product.)
* **Status:** **Active development** (as of 2024–2025). The repo is relatively new (few dozen commits) but open to contributions. It’s likely in an experimental phase internally at Google, targeting practical adoption of NetKAT ideas. This marks a bridge between academia and industry for NetKAT, with ongoing improvements expected.

## NetKAT Playground and Teaching Tools

* **Description:** In addition to the research projects above, the NetKAT community has created tools for **education and exploration** of NetKAT:

  * The **NetKAT Interactive Tutorial** (from Cornell PL group) and an online **Playground** allow users to write NetKAT programs and see their effect on example packets in a browser. These tools step through NetKAT concepts from basic filtering to advanced properties. They are built on the KATch2 verifier’s web UI, enabling real-time feedback on equivalence and inclusion checks.
  * There is a **Coq formalization** of NetKAT (in progress in the `frenetic-lang/netkat` repo) to teach and ensure the correctness of NetKAT’s equational theory. This serves as a teaching aid for formal methods courses and for researchers to experiment with proofs about network programs.
* **Language:** The playground is implemented in JavaScript/TypeScript (with WebAssembly from Rust code for verification). The Coq formalization is in Coq with some OCaml glue.
* **Repository:** The tutorial and playground are accessible via [netkat.org](https://netkat.org) (with source in the KATch2 repository). The Coq development is in [frenetic-lang/netkat](https://github.com/frenetic-lang/netkat).
* **Status:** **Active for teaching/demos.** The web-based tools are actively maintained as educational resources. They are updated alongside KATch2. The Coq project is ongoing as researchers solidify NetKAT’s metatheory. These tools are not for production use but are invaluable for learning and experimenting with NetKAT.

**Sources:**

1. NetKAT language website – research papers and tool descriptions.
2. Jules Jacobs et al., *“KATch: A Fast Symbolic Verifier for NetKAT,”* PLDI 2024 – Artifact README.
3. Jules Jacobs et al., *“StacKAT: Infinite State Network Verification,”* PLDI 2025 – Paper and online demo.
4. Steffen Smolka et al., *“McNetKAT: Scalable Verification of Probabilistic Networks,”* PLDI 2019 – Intro and artifact notes.
5. Ryan Beckett et al., *“Temporal NetKAT,”* PLDI 2016 – Artifact README and compiler code.
6. Google Open Source, *“NetKAT (C++ Implementation),”* GitHub repository README.
7. Arjun Guha et al., *“A Fast Compiler for NetKAT,”* ICFP 2015 – Talk abstract and paper artifact.
8. Homework assignment (Columbia University) – notes on NetKAT and Frenetic.
