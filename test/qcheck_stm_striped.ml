(** QCheck-STM State Machine Test for Striped Hash Map

    Uses OCaml's built-in [Hashtbl] as the sequential reference model,
    as required by the spec. *)

open QCheck
open STM

module IntKey = struct
  type t = int
  let hash k  = (Hashtbl.hash k) land (max_int - 1)
  let equal   = Int.equal
  let compare = Int.compare
end
module S = Striped_hashmap.Make(IntKey)

type cmd =
  | Insert of int * int
  | Find of int
  | Remove of int
  | Size

let show_cmd = function
  | Insert (k, v) -> Printf.sprintf "Insert(%d, %d)" k v
  | Find k -> Printf.sprintf "Find(%d)" k
  | Remove k -> Printf.sprintf "Remove(%d)" k
  | Size -> "Size"

(** Model state: a sequential [Hashtbl] *)
type model_state = (int, int) Hashtbl.t

let arb_cmd _state =
  let key_gen = Gen.int_range 0 19 in
  let val_gen = Gen.int_range 0 99 in
  QCheck.make ~print:show_cmd
    (Gen.oneof [
       Gen.map2 (fun k v -> Insert (k, v)) key_gen val_gen;
       Gen.map (fun k -> Find k) key_gen;
       Gen.map (fun k -> Remove k) key_gen;
       Gen.return Size;
     ])

(** Update the Hashtbl model according to each command *)
let next_state cmd state =
  match cmd with
  | Insert (k, v) ->
    let s = Hashtbl.copy state in
    Hashtbl.replace s k v;
    s
  | Find _ -> state
  | Remove k ->
    let s = Hashtbl.copy state in
    Hashtbl.remove s k;
    s
  | Size -> state

let precond _ _ = true

let run cmd sut =
  match cmd with
  | Insert (k, v) ->
    S.insert sut k v;
    Res (unit, ())
  | Find k ->
    Res (option int, S.find sut k)
  | Remove k ->
    Res (bool, S.remove sut k)
  | Size ->
    Res (int, S.size sut)

let postcond cmd (state : model_state) result =
  match cmd, result with
  | Insert _, Res ((Unit, _), ()) -> true
  | Find k, Res ((Option Int, _), v) ->
    let expected = Hashtbl.find_opt state k in
    v = expected
  | Remove k, Res ((Bool, _), b) ->
    let expected = Hashtbl.mem state k in
    b = expected
  | Size, Res ((Int, _), n) ->
    n = Hashtbl.length state
  | _, _ -> false

module Spec = struct
  type sut = int S.t
  type state = model_state
  type nonrec cmd = cmd

  let arb_cmd = arb_cmd
  let init_state = Hashtbl.create 16
  let next_state = next_state
  let precond = precond
  let run = run
  let init_sut () = S.create ~capacity:8 ()
  let cleanup _ = ()
  let postcond = postcond
  let show_cmd = show_cmd
end

module Seq = STM_sequential.Make(Spec)
module Dom = STM_domain.Make(Spec)

let run_sequential_test () =
  Printf.printf "Running sequential STM test...\n\n%!";
  let seq_test = Seq.agree_test ~count:1000 ~name:"Striped hashmap sequential" in
  QCheck_base_runner.run_tests ~verbose:true [seq_test]

let run_concurrent_test () =
  Printf.printf "Running concurrent STM test...\n\n%!";
  let arb_cmds_par =
    Dom.arb_triple 15 10 Spec.arb_cmd Spec.arb_cmd Spec.arb_cmd
  in
  let conc_test =
    QCheck.Test.make ~retries:10 ~count:200 ~name:"Striped hashmap concurrent"
      arb_cmds_par
    @@ fun triple -> Dom.agree_prop_par triple
  in
  QCheck_base_runner.run_tests ~verbose:true [conc_test]

let () =
  let test_name = if Array.length Sys.argv > 1 then Sys.argv.(1) else "sequential" in
  match test_name with
  | "sequential" | "seq" -> ignore (run_sequential_test ())
  | "concurrent" | "conc" -> ignore (run_concurrent_test ())
  | "all" ->
    ignore (run_sequential_test ());
    ignore (run_concurrent_test ())
  | _ ->
    Printf.eprintf "Usage: %s [sequential|concurrent|all]\n" Sys.argv.(0);
    exit 1
