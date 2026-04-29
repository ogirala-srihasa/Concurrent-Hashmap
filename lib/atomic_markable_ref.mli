(** Atomic markable reference — pair of (reference, mark-bit) with CAS. *)

type 'a t
(** An atomic reference with an associated mark bit. *)

val create : 'a -> bool -> 'a t
(** [create ref mark] creates a new atomic markable reference. *)

val get_reference : 'a t -> 'a
(** [get_reference amr] returns the current reference. *)

val get_mark : 'a t -> bool
(** [get_mark amr] returns the current mark. *)

val get : 'a t -> bool ref -> 'a
(** [get amr marked] reads both fields atomically.  Stores the mark
    in [marked] and returns the reference. *)

val compare_and_set :
  'a t ->
  expected_ref:'a -> new_ref:'a ->
  expected_mark:bool -> new_mark:bool -> bool
(** [compare_and_set amr ~expected_ref ~new_ref ~expected_mark ~new_mark]
    atomically updates both reference and mark if the current values
    match the expected ones.  Returns [true] on success. *)
