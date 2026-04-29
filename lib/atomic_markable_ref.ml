(** Atomic Markable Reference — pair of (reference, mark-bit) with CAS.

    Used for lock-free linked lists: the mark bit indicates logical deletion.
    Adapted from Lecture 07. *)

type 'a marked_ref = {
  reference : 'a;
  marked : bool;
}

type 'a t = 'a marked_ref Atomic.t

let create reference marked =
  Atomic.make { reference; marked }

let get_reference amr =
  let mr = Atomic.get amr in
  mr.reference

let get_mark amr =
  let mr = Atomic.get amr in
  mr.marked

let get amr marked =
  let mr = Atomic.get amr in
  marked := mr.marked;
  mr.reference

let compare_and_set amr ~expected_ref ~new_ref ~expected_mark ~new_mark =
  let current = Atomic.get amr in
  if current.reference == expected_ref && current.marked = expected_mark then
    let new_value = { reference = new_ref; marked = new_mark } in
    Atomic.compare_and_set amr current new_value
  else
    false
