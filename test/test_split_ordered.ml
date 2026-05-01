(** Manual tests for Split-Ordered List Hash Map *)

module IntKey = struct
  type t = int
  let hash k  = (Hashtbl.hash k) land (max_int - 1)
  let equal   = Int.equal
  let compare = Int.compare
end
module SO = Split_ordered_hashmap.Make(IntKey)

let printf = Printf.printf

(** {2 Helpers} *)

let pass msg = printf "  ✓ PASS: %s\n%!" msg
let fail msg = printf "  ✗ FAIL: %s\n%!" msg
let check condition msg = if condition then pass msg else fail msg

(** {2 Test 1 — Sequential basic operations} *)
let test_sequential () =
  printf "=== Test 1: Sequential basic operations ===\n%!";
  let t = SO.create () in
  check (SO.size t = 0) "initial size is 0";
  SO.insert t 1 10;
  SO.insert t 2 20;
  SO.insert t 3 30;
  check (SO.size t = 3) "size = 3 after 3 inserts";
  check (SO.find t 1 = Some 10) "find 1 = 10";
  check (SO.find t 2 = Some 20) "find 2 = 20";
  check (SO.find t 3 = Some 30) "find 3 = 30";
  check (SO.find t 99 = None) "find 99 = None";
  (* update *)
  SO.insert t 2 200;
  check (SO.find t 2 = Some 200) "update: find 2 = 200";
  (* remove *)
  check (SO.remove t 2 = true) "remove 2 returns true";
  check (SO.find t 2 = None) "find 2 = None after remove";
  check (SO.remove t 2 = false) "remove 2 again returns false";
  printf "\n%!"

(** {2 Test 2 — Error handling} *)
let test_errors () =
  printf "=== Test 2: Error handling ===\n%!";
  (try
     let _ = SO.create ~capacity:0 () in
     fail "should reject capacity 0"
   with Invalid_argument _ -> pass "rejects capacity 0");
  (try
     let _ = SO.create ~capacity:(-5) () in
     fail "should reject negative capacity"
   with Invalid_argument _ -> pass "rejects negative capacity");
  printf "\n%!"

(** {2 Test 3 — Concurrent inserts from multiple domains} *)
let test_concurrent_inserts () =
  printf "=== Test 3: Concurrent inserts ===\n%!";
  let t = SO.create ~capacity:8 () in
  let n_domains = 4 in
  let items_per_domain = 250 in
  let domains = List.init n_domains (fun d ->
    Domain.spawn (fun () ->
      for i = 0 to items_per_domain - 1 do
        let key = d * items_per_domain + i in
        SO.insert t key (key * 10)
      done))
  in
  List.iter Domain.join domains;
  let expected = n_domains * items_per_domain in
  check (SO.size t = expected)
    (Printf.sprintf "size = %d (expected %d)" (SO.size t) expected);
  (* verify all values *)
  let all_correct = ref true in
  for d = 0 to n_domains - 1 do
    for i = 0 to items_per_domain - 1 do
      let key = d * items_per_domain + i in
      if SO.find t key <> Some (key * 10) then
        all_correct := false
    done
  done;
  check !all_correct "all values correct";
  printf "\n%!"

(** {2 Test 4 — Concurrent mixed operations} *)
let test_concurrent_mixed () =
  printf "=== Test 4: Concurrent mixed (insert/find/remove) ===\n%!";
  let t = SO.create ~capacity:8 () in
  (* pre-populate *)
  for i = 0 to 499 do SO.insert t i i done;
  let domains = List.init 4 (fun d ->
    Domain.spawn (fun () ->
      for i = 0 to 499 do
        match d mod 3 with
        | 0 ->
          let key = 500 + (d * 500) + i in
          SO.insert t key (key * 2)
        | 1 -> ignore (SO.find t (i mod 500))
        | _ -> ignore (SO.remove t (i mod 250))
      done))
  in
  List.iter Domain.join domains;
  check true "concurrent mixed operations completed without crash";
  printf "\n%!"

(** {2 Test 5 — Incremental resize} *)
let test_resize () =
  printf "=== Test 5: Incremental resize ===\n%!";
  let t = SO.create ~capacity:4 ~load_factor:1.0 () in
  let n = 100 in
  for i = 0 to n - 1 do
    SO.insert t i (i * 100)
  done;
  check (SO.size t = n)
    (Printf.sprintf "size = %d after many inserts (expected %d)" (SO.size t) n);
  (* verify all entries survived resize *)
  let all_found = ref true in
  for i = 0 to n - 1 do
    if SO.find t i <> Some (i * 100) then
      all_found := false
  done;
  check !all_found "all entries survived incremental resize";
  printf "\n%!"

(** {2 Test 6 — Concurrent resize stress} *)
let test_concurrent_resize () =
  printf "=== Test 6: Concurrent resize stress ===\n%!";
  let t = SO.create ~capacity:4 ~load_factor:0.5 () in
  let n = 100 in
  let domains = List.init 2 (fun d ->
    Domain.spawn (fun () ->
      for i = 0 to n - 1 do
        let key = d * n + i in
        SO.insert t key key
      done))
  in
  List.iter Domain.join domains;
  let expected = 2 * n in
  check (SO.size t = expected)
    (Printf.sprintf "size = %d (expected %d)" (SO.size t) expected);
  printf "\n%!"

(** {2 Test 7 — Negative integer keys} *)
let test_negative_keys () =
  printf "=== Test 7: Negative integer keys ===\n%!";
  let t = SO.create () in
  (* basic negative key operations *)
  SO.insert t (-1) 100;
  SO.insert t (-5) 500;
  SO.insert t 5 50;
  check (SO.find t (-1) = Some 100) "find(-1) = 100";
  check (SO.find t (-5) = Some 500) "find(-5) = 500";
  check (SO.find t 5 = Some 50) "find(5) = 50";
  check (SO.size t = 3) "size = 3 after 3 inserts (mixed signs)";
  (* min_int vs 0 — these must not collide *)
  let t2 = SO.create () in
  SO.insert t2 0 999;
  SO.insert t2 min_int 111;
  check (SO.find t2 0 = Some 999) "find(0) = 999 (no min_int collision)";
  check (SO.find t2 min_int = Some 111) "find(min_int) = 111";
  check (SO.size t2 = 2) "size = 2 (0 and min_int are distinct)";
  (* remove negative key *)
  check (SO.remove t (-5) = true) "remove(-5) returns true";
  check (SO.find t (-5) = None) "find(-5) = None after remove";
  check (SO.find t 5 = Some 50) "find(5) still intact after removing -5";
  printf "\n%!"

(** {2 Test 8 — String keys} *)
module StringKey = struct
  type t = string
  let hash    = Hashtbl.hash
  let equal   = String.equal
  let compare = String.compare
end
module SOS = Split_ordered_hashmap.Make(StringKey)

let test_string_keys () =
  printf "=== Test 8: String keys ===\n%!";
  let t = SOS.create () in
  SOS.insert t "hello" 1;
  SOS.insert t "world" 2;
  SOS.insert t "foo" 3;
  check (SOS.size t = 3) "size = 3 after 3 string inserts";
  check (SOS.find t "hello" = Some 1) "find \"hello\" = 1";
  check (SOS.find t "world" = Some 2) "find \"world\" = 2";
  check (SOS.find t "foo" = Some 3) "find \"foo\" = 3";
  check (SOS.find t "bar" = None) "find \"bar\" = None";
  (* update *)
  SOS.insert t "hello" 99;
  check (SOS.find t "hello" = Some 99) "update: find \"hello\" = 99";
  check (SOS.size t = 3) "size unchanged after update";
  (* remove *)
  check (SOS.remove t "world" = true) "remove \"world\" returns true";
  check (SOS.find t "world" = None) "find \"world\" = None after remove";
  check (SOS.remove t "world" = false) "remove \"world\" again returns false";
  check (SOS.size t = 2) "size = 2 after remove";
  (* empty string key *)
  SOS.insert t "" 42;
  check (SOS.find t "" = Some 42) "find \"\" = 42 (empty string key)";
  check (SOS.size t = 3) "size = 3 after inserting empty string key";
  printf "\n%!"

(** {2 Test 9 — Float keys} *)
module FloatKey = struct
  type t = float
  let hash    = Hashtbl.hash
  let equal   = Float.equal
  let compare = Float.compare
end
module SOF = Split_ordered_hashmap.Make(FloatKey)

let test_float_keys () =
  printf "=== Test 9: Float keys ===\n%!";
  let t = SOF.create () in
  SOF.insert t 1.0 "one";
  SOF.insert t 2.5 "two-point-five";
  SOF.insert t (-3.14) "neg-pi";
  check (SOF.size t = 3) "size = 3 after 3 float inserts";
  check (SOF.find t 1.0 = Some "one") "find 1.0 = \"one\"";
  check (SOF.find t 2.5 = Some "two-point-five") "find 2.5 = \"two-point-five\"";
  check (SOF.find t (-3.14) = Some "neg-pi") "find (-3.14) = \"neg-pi\"";
  check (SOF.find t 999.0 = None) "find 999.0 = None";
  (* update *)
  SOF.insert t 1.0 "ONE";
  check (SOF.find t 1.0 = Some "ONE") "update: find 1.0 = \"ONE\"";
  check (SOF.size t = 3) "size unchanged after update";
  (* remove *)
  check (SOF.remove t 2.5 = true) "remove 2.5 returns true";
  check (SOF.find t 2.5 = None) "find 2.5 = None after remove";
  check (SOF.size t = 2) "size = 2 after remove";
  (* 0.0 vs -0.0 — Float.equal treats them as equal *)
  let t2 = SOF.create () in
  SOF.insert t2 0.0 "zero";
  SOF.insert t2 (-0.0) "neg-zero";
  check (SOF.find t2 0.0 = Some "neg-zero") "0.0 and -0.0 are equal (Float.equal)";
  check (SOF.size t2 = 1) "size = 1 (0.0 and -0.0 are the same key)";
  printf "\n%!"

(** {2 Main} *)
let () =
  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n%!";
  printf "  Split-Ordered List Hash Map — Manual Test Suite\n%!";
  printf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n%!";
  test_sequential ();
  test_errors ();
  test_concurrent_inserts ();
  test_concurrent_mixed ();
  test_resize ();
  test_concurrent_resize ();
  test_negative_keys ();
  test_string_keys ();
  test_float_keys ();
  printf "All split-ordered hash map tests completed.\n%!"
