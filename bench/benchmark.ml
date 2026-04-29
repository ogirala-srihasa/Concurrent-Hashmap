(** Benchmark suite for Concurrent Hash Maps

    Measures throughput under three workload profiles across 1-8 threads:
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

(** {2 Helpers} *)

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

(** {2 Workload profiles} *)

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

let thread_counts = [1; 2; 4; 8]

(** {2 Main benchmark} *)

let run_throughput_benchmarks () =
  let ops_per_thread = 50_000 in
  let initial_size = 1_000 in
  let capacity = 16 in

  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n%!";
  printf "  Throughput Benchmark (ops/thread = %d, initial_size = %d)\n%!" ops_per_thread initial_size;
  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n%!";

  List.iter (fun wl ->
    printf "--- %s ---\n%!" wl.name;
    printf "%-14s  %-10s  %-12s  %-10s  %-12s\n%!"
      "Threads" "Striped(s)" "Striped(ops/s)" "Split(s)" "Split(ops/s)";
    printf "%s\n%!" (String.make 70 '-');
    List.iter (fun threads ->
      let (se, st) = bench_striped ~threads ~ops_per_thread
        ~find_pct:wl.find_pct ~insert_pct:wl.insert_pct ~remove_pct:wl.remove_pct
        ~initial_size ~capacity
      in
      let (soe, sot) = bench_split ~threads ~ops_per_thread
        ~find_pct:wl.find_pct ~insert_pct:wl.insert_pct ~remove_pct:wl.remove_pct
        ~initial_size ~capacity
      in
      printf "%-14d  %-10.4f  %-12.0f  %-10.4f  %-12.0f\n%!"
        threads se st soe sot)
    thread_counts;
    printf "\n%!")
  workloads

(** {2 Resize benchmark} *)

let run_resize_benchmark () =
  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n%!";
  printf "  Resize Benchmark — throughput during active resizing\n%!";
  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n%!";

  let ops_per_thread = 20_000 in
  printf "%-14s  %-10s  %-12s  %-10s  %-12s\n%!"
    "Threads" "Striped(s)" "Striped(ops/s)" "Split(s)" "Split(ops/s)";
  printf "%s\n%!" (String.make 70 '-');
  List.iter (fun threads ->
    (* start small, force many resizes *)
    let (se, st) = bench_striped ~threads ~ops_per_thread
      ~find_pct:10 ~insert_pct:80 ~remove_pct:10
      ~initial_size:0 ~capacity:4
    in
    let (soe, sot) = bench_split ~threads ~ops_per_thread
      ~find_pct:10 ~insert_pct:80 ~remove_pct:10
      ~initial_size:0 ~capacity:4
    in
    printf "%-14d  %-10.4f  %-12.0f  %-10.4f  %-12.0f\n%!"
      threads se st soe sot)
  thread_counts;
  printf "\n%!"

(** {2 Memory overhead} *)

let run_memory_benchmark () =
  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n%!";
  printf "  Memory Overhead Comparison\n%!";
  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n%!";

  let n = 10_000 in

  Gc.compact ();
  let before_striped = Gc.stat () in
  let ts = Striped.create ~capacity:16 () in
  for i = 0 to n - 1 do Striped.insert ts i i done;
  Gc.compact ();
  let after_striped = Gc.stat () in

  Gc.compact ();
  let before_split = Gc.stat () in
  let tc = Split.create ~capacity:16 () in
  for i = 0 to n - 1 do Split.insert tc i i done;
  Gc.compact ();
  let after_split = Gc.stat () in

  let word_size = Sys.word_size / 8 in
  let striped_words = after_striped.Gc.live_words - before_striped.Gc.live_words in
  let split_words = after_split.Gc.live_words - before_split.Gc.live_words in

  printf "  After inserting %d items:\n%!" n;
  printf "    Striped:       %d live words (%d KB)\n%!" striped_words (striped_words * word_size / 1024);
  printf "    Split-ordered: %d live words (%d KB)\n%!" split_words (split_words * word_size / 1024);
  printf "    Ratio (split/striped): %.2fx\n\n%!" (float_of_int split_words /. float_of_int striped_words);

  (* keep references alive so GC doesn't collect early *)
  ignore (Striped.size ts);
  ignore (Split.size tc)

(** {2 Main} *)

let () =
  printf "\n%!";
  run_throughput_benchmarks ();
  run_resize_benchmark ();
  run_memory_benchmark ();
  printf "Benchmark complete.\n%!"
