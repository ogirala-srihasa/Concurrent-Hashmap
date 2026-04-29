(** QCheck-Lin Linearizability Test for Striped Hash Map *)

open Lin

module IntKey = struct
  type t = int
  let hash k  = (Hashtbl.hash k) land (max_int - 1)
  let equal   = Int.equal
  let compare = Int.compare
end
module S = Striped_hashmap.Make(IntKey)

module StripedSig = struct
  type t = int S.t

  let init () = S.create ~capacity:8 ()
  let cleanup _ = ()

  let int_small = int_small

  (* Note: size is excluded because the counter increment is a
     separate atomic operation from the structural mutation, creating
     an inherent non-linearizable window.  Size correctness is
     verified by QCheck-STM sequential tests instead. *)
  let api =
    [ val_ "insert" S.insert
        (t @-> int_small @-> int_small @-> returning unit);
      val_ "find" S.find
        (t @-> int_small @-> returning (option int));
      val_ "remove" S.remove
        (t @-> int_small @-> returning bool);
    ]
end

module Striped_domain = Lin_domain.Make(StripedSig)

let () =
  QCheck_base_runner.run_tests_main [
    Striped_domain.lin_test ~count:1000 ~name:"Striped hashmap linearizability";
  ]
