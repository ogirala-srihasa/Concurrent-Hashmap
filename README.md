### Deliverables

Every project must produce three deliverables:

1. **Implementation** — working code for a concurrency problem not covered in
   lectures, with tests and/or evaluation demonstrating correctness and
   performance.
2. **Written report** — 5–10 pages, LaTeX (template will be shared). The report
   should cover: goals of the project, tasks undertaken, evaluation, and
   conclusions. The report **must** list the contributions of each group member
   in terms of percentages.
3. **Presentation** — 10-minute presentation + Q&A. Only one group member needs
   to present.

Code and report must be submitted as a single GitHub repository. The report
must be written in LaTeX, Markdown, or another open, machine-readable format.

### Grading Rubric (out of 16 marks)

| Component | Marks | What we look for |
|---|---|---|
| Challenge of the problem undertaken | 3 | Ambition, novelty, relevance to course topics. Each project below has a difficulty rating (★ to ★★★★★). **Higher-difficulty projects receive higher marks in this component even if completion is partial.** |
| Progress made towards the challenge | 5 | Working implementation, depth of evaluation, evidence of effort |
| Written report | 5 | Clarity, technical depth, proper evaluation, contribution breakdown |
| Presentation | 3 | Clear explanation, good use of time, ability to answer questions |

---

## Project 10: Concurrent Hash Map — Striped Locking vs. Split-Ordered Lists

**Difficulty: ★★★★☆** — Striped locking is moderate, but lock-free split-ordered lists with incremental resizing are very challenging.

### Background

Hash maps are among the most widely used data structures, and making them
concurrent is a core challenge in practice. Two major approaches exist:
**striped locking**, where the bucket array is partitioned into lock stripes
(each stripe is a mutex protecting a subset of buckets), and **split-ordered
lists** (Shalev & Shavit, 2006), a lock-free design that represents the hash
table as a single sorted linked list with sentinel nodes, enabling **lock-free
resizing** without rehashing. The key insight of split-ordered lists is to store
keys in bit-reversed order so that bucket splits never require moving items. Both
approaches connect directly to the concurrent linked list techniques from
Lecture 07 and the locking strategies from Lectures 05–06.

### Tasks

1. Implement a **striped concurrent hash map** in OCaml 5: use an array of
   `Mutex`-protected buckets with a configurable stripe count. Support `insert`,
   `remove`, and `find`. Implement **dynamic resizing**: when the load factor
   exceeds a threshold, acquire all stripes, allocate a new bucket array, and
   rehash.
2. Implement a **lock-free hash map using split-ordered lists** in OCaml 5. The
   table should consist of a single lock-free linked list (using
   `Atomic.compare_and_set` with logical deletion as in Lecture 07) plus an
   expandable array of sentinel pointers. Implement **incremental resizing** by
   lazily initialising new sentinel nodes as the table grows.
3. Verify correctness of both implementations using **QCheck-Lin** and
   **QCheck-STM**: model the hash map as a sequential `Hashtbl` and test
   concurrent `insert`/`remove`/`find` sequences for linearizability
   (QCheck-Lin) and sequential specification conformance (QCheck-STM).
4. Run **TSAN** on both implementations.
5. Benchmark throughput under three workload profiles: read-heavy (90% `find`),
   write-heavy (90% `insert`/`remove`), and mixed (50/25/25), across 1–8
   threads. Also measure throughput **during active resizing** to evaluate how
   each design handles growth under load.
6. Measure and compare **memory overhead** of the two approaches: the striped
   design wastes space on locks per stripe, while split-ordered lists add
   sentinel nodes.

### Research Question

Does the lock-free split-ordered list design provide meaningful throughput
advantages over striped locking under read-heavy workloads, and how do the two
approaches compare during concurrent resizing?

### References

- O. Shalev, N. Shavit, "Split-Ordered Lists: Lock-Free Extensible Hash Tables," *JACM*, 2006
- AoMPP Chapter 13 (Concurrent Hashing)
- Java `ConcurrentHashMap` source code (striped locking reference)

---