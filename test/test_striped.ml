(** Manual tests for Striped Concurrent Hash Map *)

module IntKey = struct
  type t = int
  let hash k  = (Hashtbl.hash k) land (max_int - 1)
  let equal   = Int.equal
  let compare = Int.compare
end
module S = Striped_hashmap.Make(IntKey)

let printf = Printf.printf

(** {Helpers} *)

let pass msg = printf "  ✓ PASS: %s\n%!" msg
let fail msg = printf "  ✗ FAIL: %s\n%!" msg
let check condition msg = if condition then pass msg else fail msg

(** { Test 1 — Sequential basic operations} *)
let test_sequential () =
  printf "=== Test 1: Sequential basic operations ===\n%!";
  let t = S.create () in
  check (S.size t = 0) "initial size is 0";
  S.insert t 1 10;
  S.insert t 2 20;
  S.insert t 3 30;
  check (S.size t = 3) "size = 3 after 3 inserts";
  check (S.find t 1 = Some 10) "find 1 = 10";
  check (S.find t 2 = Some 20) "find 2 = 20";
  check (S.find t 3 = Some 30) "find 3 = 30";
  check (S.find t 99 = None) "find 99 = None";
  (* update *)
  S.insert t 2 200;
  check (S.find t 2 = Some 200) "update: find 2 = 200";
  check (S.size t = 3) "size unchanged after update";
  (* remove *)
  check (S.remove t 2 = true) "remove 2 returns true";
  check (S.find t 2 = None) "find 2 = None after remove";
  check (S.remove t 2 = false) "remove 2 again returns false";
  check (S.size t = 2) "size = 2 after remove";
  printf "\n%!"

(** {Test 2 — Error handling} *)
let test_errors () =
  printf "=== Test 2: Error handling ===\n%!";
  (try
     let _ = S.create ~capacity:0 () in
     fail "should reject capacity 0"
   with Invalid_argument _ -> pass "rejects capacity 0");
  (try
     let _ = S.create ~num_stripes:(-1) () in
     fail "should reject negative stripes"
   with Invalid_argument _ -> pass "rejects negative stripes");
  printf "\n%!"

(** {Test 3 — Concurrent inserts from multiple domains} *)
let test_concurrent_inserts () =
  printf "=== Test 3: Concurrent inserts ===\n%!";
  let t = S.create ~capacity:8 () in
  let n_domains = 4 in
  let items_per_domain = 500 in
  let domains = List.init n_domains (fun d ->
    Domain.spawn (fun () ->
      for i = 0 to items_per_domain - 1 do
        let key = d * items_per_domain + i in
        S.insert t key (key * 10)
      done))
  in
  List.iter Domain.join domains;
  let expected = n_domains * items_per_domain in
  check (S.size t = expected)
    (Printf.sprintf "size = %d (expected %d)" (S.size t) expected);
  (* verify all values *)
  let all_correct = ref true in
  for d = 0 to n_domains - 1 do
    for i = 0 to items_per_domain - 1 do
      let key = d * items_per_domain + i in
      if S.find t key <> Some (key * 10) then
        all_correct := false
    done
  done;
  check !all_correct "all values correct";
  printf "\n%!"

(** {Test 4 — Concurrent mixed operations} *)
let test_concurrent_mixed () =
  printf "=== Test 4: Concurrent mixed (insert/find/remove) ===\n%!";
  let t = S.create ~capacity:8 () in
  (* pre-populate *)
  for i = 0 to 999 do S.insert t i i done;
  let domains = List.init 4 (fun d ->
    Domain.spawn (fun () ->
      for i = 0 to 999 do
        let key = (d * 1000) + i in
        match d mod 3 with
        | 0 -> S.insert t key (key * 2)
        | 1 -> ignore (S.find t (i mod 1000))
        | _ -> ignore (S.remove t (i mod 500))
      done))
  in
  List.iter Domain.join domains;
  check true "concurrent mixed operations completed without crash";
  printf "\n%!"

(** {Test 5 — Resize correctness under load} *)
let test_resize () =
  printf "=== Test 5: Resize under load ===\n%!";
  let t = S.create ~capacity:4 ~num_stripes:4 ~load_factor:0.5 () in
  let initial_cap = S.capacity t in
  check (initial_cap = 4)
    (Printf.sprintf "initial capacity = %d (expected 4)" initial_cap);
  let n = 200 in
  let domains = List.init 2 (fun d ->
    Domain.spawn (fun () ->
      for i = 0 to n - 1 do
        let key = d * n + i in
        S.insert t key key
      done))
  in
  List.iter Domain.join domains;
  let expected = 2 * n in
  check (S.size t = expected)
    (Printf.sprintf "size = %d after resize (expected %d)" (S.size t) expected);
  (* verify capacity actually grew — with 400 items and load_factor 0.5,
     we need at least 800 buckets, so capacity must have grown well past 4 *)
  let final_cap = S.capacity t in
  check (final_cap > initial_cap)
    (Printf.sprintf "capacity grew: %d -> %d" initial_cap final_cap);
  check (final_cap >= expected * 2)
    (Printf.sprintf "capacity >= 2*size: %d >= %d" final_cap (expected * 2));
  (* verify all entries survived resize *)
  let all_found = ref true in
  for d = 0 to 1 do
    for i = 0 to n - 1 do
      let key = d * n + i in
      if S.find t key <> Some key then all_found := false
    done
  done;
  check !all_found "all entries survived resize";
  printf "\n%!"

(** {Test 6 — No lost or duplicated items} *)
let test_no_lost_items () =
  printf "=== Test 6: No lost or duplicated items ===\n%!";
  let t = S.create () in
  let n = 1000 in
  (* insert 0..999 from two domains simultaneously *)
  let d1 = Domain.spawn (fun () ->
    for i = 0 to n - 1 do S.insert t i i done)
  in
  let d2 = Domain.spawn (fun () ->
    for i = 0 to n - 1 do S.insert t i i done)
  in
  Domain.join d1; Domain.join d2;
  check (S.size t = n)
    (Printf.sprintf "size = %d (expected %d, no duplicates)" (S.size t) n);
  printf "\n%!"

(** {Test 7 — Negative integer keys} *)
let test_negative_keys () =
  printf "=== Test 7: Negative integer keys ===\n%!";
  let t = S.create () in
  (* basic negative key operations *)
  S.insert t (-1) 100;
  S.insert t (-5) 500;
  S.insert t 5 50;
  check (S.find t (-1) = Some 100) "find(-1) = 100";
  check (S.find t (-5) = Some 500) "find(-5) = 500";
  check (S.find t 5 = Some 50) "find(5) = 50";
  check (S.size t = 3) "size = 3 after 3 inserts (mixed signs)";
  (* min_int vs 0 — these must not collide *)
  let t2 = S.create () in
  S.insert t2 0 999;
  S.insert t2 min_int 111;
  check (S.find t2 0 = Some 999) "find(0) = 999 (no min_int collision)";
  check (S.find t2 min_int = Some 111) "find(min_int) = 111";
  check (S.size t2 = 2) "size = 2 (0 and min_int are distinct)";
  (* remove negative key *)
  check (S.remove t (-5) = true) "remove(-5) returns true";
  check (S.find t (-5) = None) "find(-5) = None after remove";
  check (S.find t 5 = Some 50) "find(5) still intact after removing -5";
  printf "\n%!"

(** {Main} *)
let () =
  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n%!";
  printf "  Striped Hash Map — Manual Test Suite\n%!";
  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n%!";
  test_sequential ();
  test_errors ();
  test_concurrent_inserts ();
  test_concurrent_mixed ();
  test_resize ();
  test_no_lost_items ();
  test_negative_keys ();
  printf "All striped hash map tests completed.\n%!"
