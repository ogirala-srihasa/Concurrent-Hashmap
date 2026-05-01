(** Benchmark suite for Concurrent Hash Maps

    Measures throughput under three workload profiles across 1-8 threads.
    Each configuration is run 10 times and the average is reported.
      - Read-heavy   (90% find, 5% insert, 5% remove)
      - Write-heavy  (45% insert, 45% remove, 10% find)
      - Mixed        (50% find, 25% insert, 25% remove)

    Also measures throughput during active resizing and memory overhead. *)

module IntKey = struct
  type t = int
  let hash k  = (Hashtbl.hash k) land (max_int - 1)
  let equal   = Int.equal
  let compare = Int.compare
end
module Striped = Striped_hashmap.Make(IntKey)
module Split   = Split_ordered_hashmap.Make(IntKey)

let printf = Printf.printf

(** {Helpers} *)

let num_runs = 10

(** Run a single benchmark for the striped hash map *)
let bench_striped ~threads ~ops_per_thread ~find_pct ~insert_pct ~remove_pct:_
    ~initial_size ~capacity =
  let t = Striped.create ~capacity () in
  (* pre-populate *)
  for i = 0 to initial_size - 1 do
    Striped.insert t i i
  done;
  let start = Unix.gettimeofday () in
  let domains = List.init threads (fun d ->
    Domain.spawn (fun () ->
      let state = Random.State.make [| d; 42 |] in
      for _ = 1 to ops_per_thread do
        let r = Random.State.int state 100 in
        let key = Random.State.int state (max 1 (initial_size * 2)) in
        if r < find_pct then
          ignore (Striped.find t key)
        else if r < find_pct + insert_pct then
          Striped.insert t key (key * 10)
        else
          ignore (Striped.remove t key)
      done))
  in
  List.iter Domain.join domains;
  let elapsed = Unix.gettimeofday () -. start in
  let total_ops = threads * ops_per_thread in
  let throughput = float_of_int total_ops /. elapsed in
  (elapsed, throughput)

(** Run a single benchmark for the split-ordered hash map *)
let bench_split ~threads ~ops_per_thread ~find_pct ~insert_pct ~remove_pct:_
    ~initial_size ~capacity =
  let t = Split.create ~capacity () in
  (* pre-populate *)
  for i = 0 to initial_size - 1 do
    Split.insert t i i
  done;
  let start = Unix.gettimeofday () in
  let domains = List.init threads (fun d ->
    Domain.spawn (fun () ->
      let state = Random.State.make [| d; 42 |] in
      for _ = 1 to ops_per_thread do
        let r = Random.State.int state 100 in
        let key = Random.State.int state (max 1 (initial_size * 2)) in
        if r < find_pct then
          ignore (Split.find t key)
        else if r < find_pct + insert_pct then
          Split.insert t key (key * 10)
        else
          ignore (Split.remove t key)
      done))
  in
  List.iter Domain.join domains;
  let elapsed = Unix.gettimeofday () -. start in
  let total_ops = threads * ops_per_thread in
  let throughput = float_of_int total_ops /. elapsed in
  (elapsed, throughput)

(** Run a benchmark [num_runs] times and return the average (elapsed, throughput) *)
let avg_bench bench_fn ~threads ~ops_per_thread ~find_pct ~insert_pct ~remove_pct
    ~initial_size ~capacity =
  let total_elapsed = ref 0.0 in
  let total_throughput = ref 0.0 in
  for _ = 1 to num_runs do
    let (e, t) = bench_fn ~threads ~ops_per_thread ~find_pct ~insert_pct ~remove_pct
      ~initial_size ~capacity in
    total_elapsed := !total_elapsed +. e;
    total_throughput := !total_throughput +. t
  done;
  let n = float_of_int num_runs in
  (!total_elapsed /. n, !total_throughput /. n)

(** {Workload profiles} *)

type workload = {
  name : string;
  find_pct : int;
  insert_pct : int;
  remove_pct : int;
}

let workloads = [
  { name = "Read-heavy  (90/5/5)"; find_pct = 90; insert_pct = 5; remove_pct = 5 };
  { name = "Write-heavy (10/45/45)"; find_pct = 10; insert_pct = 45; remove_pct = 45 };
  { name = "Mixed       (50/25/25)"; find_pct = 50; insert_pct = 25; remove_pct = 25 };
]

let thread_counts = [1; 2; 3; 4; 5; 6; 7; 8]

(** {Main benchmark} *)

let run_throughput_benchmarks () =
  let ops_per_thread = 500_000 in
  let initial_size = 1_000 in
  let capacity = 16 in

  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n%!";
  printf "  Throughput Benchmark (ops/thread = %d, initial_size = %d, avg of %d runs)\n%!"
    ops_per_thread initial_size num_runs;
  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n%!";

  List.iter (fun wl ->
    printf "--- %s ---\n%!" wl.name;
    printf "%-10s  %-12s  %-14s  %-12s  %-14s\n%!"
      "Threads" "Striped(s)" "Striped(ops/s)" "Split(s)" "Split(ops/s)";
    printf "%s\n%!" (String.make 74 '-');
    List.iter (fun threads ->
      let (se, st) = avg_bench bench_striped ~threads ~ops_per_thread
        ~find_pct:wl.find_pct ~insert_pct:wl.insert_pct ~remove_pct:wl.remove_pct
        ~initial_size ~capacity
      in
      let (soe, sot) = avg_bench bench_split ~threads ~ops_per_thread
        ~find_pct:wl.find_pct ~insert_pct:wl.insert_pct ~remove_pct:wl.remove_pct
        ~initial_size ~capacity
      in
      printf "%-10d  %-12.4f  %-14.0f  %-12.4f  %-14.0f\n%!"
        threads se st soe sot)
    thread_counts;
    printf "\n%!")
  workloads

(** {Resize benchmark} *)

let run_resize_benchmark () =
  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n%!";
  printf "  Resize Benchmark — throughput during active resizing (avg of %d runs)\n%!" num_runs;
  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n%!";

  let ops_per_thread = 200_000 in
  printf "%-10s  %-12s  %-14s  %-12s  %-14s\n%!"
    "Threads" "Striped(s)" "Striped(ops/s)" "Split(s)" "Split(ops/s)";
  printf "%s\n%!" (String.make 74 '-');
  List.iter (fun threads ->
    (* start small, force many resizes *)
    let (se, st) = avg_bench bench_striped ~threads ~ops_per_thread
      ~find_pct:10 ~insert_pct:80 ~remove_pct:10
      ~initial_size:0 ~capacity:4
    in
    let (soe, sot) = avg_bench bench_split ~threads ~ops_per_thread
      ~find_pct:10 ~insert_pct:80 ~remove_pct:10
      ~initial_size:0 ~capacity:4
    in
    printf "%-10d  %-12.4f  %-14.0f  %-12.4f  %-14.0f\n%!"
      threads se st soe sot)
  thread_counts;
  printf "\n%!"

(** {Memory overhead} *)

(** Measure live words consumed by [n] insertions into a fresh map.
    We compact before and after to get an accurate delta.
    The map reference is returned so the GC doesn't collect it early. *)
let measure_memory_striped n =
  Gc.compact ();
  let before = Gc.stat () in
  let t = Striped.create ~capacity:16 () in
  for i = 0 to n - 1 do Striped.insert t i i done;
  Gc.compact ();
  let after = Gc.stat () in
  let words = after.Gc.live_words - before.Gc.live_words in
  (words, t)

let measure_memory_split n =
  Gc.compact ();
  let before = Gc.stat () in
  let t = Split.create ~capacity:16 () in
  for i = 0 to n - 1 do Split.insert t i i done;
  Gc.compact ();
  let after = Gc.stat () in
  let words = after.Gc.live_words - before.Gc.live_words in
  (words, t)

let run_memory_benchmark () =
  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n%!";
  printf "  Memory Overhead Comparison\n%!";
  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n%!";

  let word_size = Sys.word_size / 8 in
  let sizes = [100; 1_000; 10_000; 100_000; 500_000] in

  printf "%-10s  %-16s  %-16s  %-16s  %-16s  %-8s\n%!"
    "N" "Striped(words)" "Striped(KB)" "Split(words)" "Split(KB)" "Ratio";
  printf "%s\n%!" (String.make 90 '-');

  List.iter (fun n ->
    let (sw, ts) = measure_memory_striped n in
    let (spw, tc) = measure_memory_split n in
    let ratio = if sw > 0 then float_of_int spw /. float_of_int sw else 0.0 in
    printf "%-10d  %-16d  %-16d  %-16d  %-16d  %-8.2f\n%!"
      n sw (sw * word_size / 1024) spw (spw * word_size / 1024) ratio;
    (* keep references alive so GC doesn't collect early *)
    ignore (Striped.size ts);
    ignore (Split.size tc))
  sizes;
  printf "\n%!"

(** {Main} *)

let () =
  printf "\n%!";
  run_throughput_benchmarks ();
  run_resize_benchmark ();
  run_memory_benchmark ();
  printf "Benchmark complete.\n%!"
