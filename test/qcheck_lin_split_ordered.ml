(** QCheck-Lin Linearizability Test for Split-Ordered Hash Map *)

open Lin

module IntKey = struct
  type t = int
  let hash k  = (Hashtbl.hash k) land (max_int - 1)
  let equal   = Int.equal
  let compare = Int.compare
end
module SO = Split_ordered_hashmap.Make(IntKey)

module SplitSig = struct
  type t = int SO.t

  let init () = SO.create ~capacity:8 ()
  let cleanup _ = ()

  let int_small = int_small

  (* Note: size is excluded because the lock-free implementation
     increments the counter separately from the list insertion CAS,
     creating an inherent non-linearizable window.  Size correctness
     is verified by QCheck-STM sequential tests instead. *)
  let api =
    [ val_ "insert" SO.insert
        (t @-> int_small @-> int_small @-> returning unit);
      val_ "find" SO.find
        (t @-> int_small @-> returning (option int));
      val_ "remove" SO.remove
        (t @-> int_small @-> returning bool);
    ]
end

module Split_domain = Lin_domain.Make(SplitSig)

let () =
  QCheck_base_runner.run_tests_main [
    Split_domain.lin_test ~count:1000 ~name:"Split-ordered hashmap linearizability";
  ]
