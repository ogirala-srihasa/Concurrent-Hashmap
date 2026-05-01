# Concurrent Hash Maps in OCaml 5

[presentation link](https://youtu.be/J9f-dlMlk3U)

A comparative implementation of two concurrent hash map designs in OCaml 5's multicore runtime:

- **Striped Locking** — mutex-protected bucket stripes with stop-the-world resizing
- **Split-Ordered Lists** — a lock-free design using Harris-style linked lists with incremental resizing

Based on Chapter 13 of *The Art of Multiprocessor Programming* (Herlihy & Shavit) and the Shalev & Shavit 2006 JACM paper.

## Project Structure

```
ConcurrentHashmap/
├── lib/                              # Core library
│   ├── hashmap_intf.ml               # KEY module type + HASHMAP signature
│   ├── atomic_markable_ref.ml/.mli   # Lock-free markable pointer (Harris-style)
│   ├── striped_hashmap.ml/.mli       # Striped locking implementation
│   └── split_ordered_hashmap.ml/.mli # Split-ordered list implementation
├── test/                             # Test suite
│   ├── test_striped.ml               # Manual tests (9 tests, incl. string/float keys)
│   ├── test_split_ordered.ml         # Manual tests (9 tests, incl. string/float keys)
│   ├── qcheck_lin_striped.ml         # QCheck-Lin linearizability tests
│   ├── qcheck_lin_split_ordered.ml   # QCheck-Lin linearizability tests
│   ├── qcheck_stm_striped.ml        # QCheck-STM state-machine tests
│   └── qcheck_stm_split_ordered.ml  # QCheck-STM state-machine tests
├── bench/
│   └── benchmark.ml                  # Throughput, resize, and memory benchmarks
├── report/                  # LaTeX report (CS6868 submission)
│   ├── main.tex
│   └── references.bib                 
├── Makefile
└── dune-project
```

## Prerequisites

- **OCaml 5.x** with multicore support
- **opam** packages: `qcheck-lin`, `qcheck-stm`, `qcheck-multicoretests-util`
- (Optional) OCaml `5.4.0+tsan` switch for ThreadSanitizer data-race detection

## Building

```sh
make build
```

## Running Tests

```sh
# Run all tests (manual + QCheck-Lin + QCheck-STM)
make test

# Individual test suites
make test-striped           # Manual tests for striped hash map
make test-split             # Manual tests for split-ordered hash map
make test-lin-striped       # Linearizability tests (striped)
make test-lin-split         # Linearizability tests (split-ordered)
make test-stm-striped-seq   # STM sequential tests (striped)
make test-stm-striped-conc  # STM concurrent tests (striped)
make test-stm-split-seq     # STM sequential tests (split-ordered)
make test-stm-split-conc    # STM concurrent tests (split-ordered)
```

### TSAN (ThreadSanitizer)

```sh
opam switch 5.4.0+tsan
eval $(opam env)
make build
make test-striped
make test-split
```

## Benchmarks

```sh
make bench
```

Runs throughput benchmarks across 1–8 threads under three workload profiles (read-heavy, write-heavy, mixed), a resize stress benchmark, and a memory overhead comparison across different map sizes. Each configuration is averaged over 10 runs.

## API

Both implementations expose the same interface via `Make(K : KEY)` functors:

```ocaml
module IntKey = struct
  type t = int
  let hash k  = (Hashtbl.hash k) land (max_int - 1)
  let equal   = Int.equal
  let compare = Int.compare
end

module MyMap = Striped_hashmap.Make(IntKey)
(* or *)
module MyMap = Split_ordered_hashmap.Make(IntKey)

let t = MyMap.create ~capacity:16 ()
let () = MyMap.insert t 42 "hello"
let v  = MyMap.find t 42          (* Some "hello" *)
let b  = MyMap.remove t 42        (* true *)
let n  = MyMap.size t             (* 0 *)
```

## Design Highlights

| Feature | Striped | Split-Ordered |
|---|---|---|
| Synchronisation | Mutex stripes | Lock-free (CAS) |
| `find` | Acquires stripe lock | Wait-free (atomic reads only) |
| Resize | Stop-the-world (all locks) | O(1) CAS + lazy sentinel init |
| Value update | In-place under lock | Atomic.set on value field |
| Load factor default | 0.75 | 2.0 |
| Memory overhead | ~1× baseline | ~1.7× at scale |


## References

- O. Shalev, N. Shavit, "Split-Ordered Lists: Lock-Free Extensible Hash Tables," *JACM*, 2006
- M. Herlihy, N. Shavit, *The Art of Multiprocessor Programming*, 2nd ed., 2020 — Chapter 13