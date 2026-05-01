(** Split-Ordered List Hash Map — Lock-Free with Incremental Resizing

    Based on Shalev & Shavit, "Split-Ordered Lists: Lock-Free Extensible
    Hash Tables" (JACM 2006).

    Key insight: keys are stored in bit-reversed order in a single sorted
    lock-free linked list.  Bucket splits during resizing never move items.

    Sentinel nodes mark bucket boundaries; regular nodes hold data.  Bucket
    sentinels are created lazily on first access.

    Uses Harris-style lock-free list operations (mark → CAS remove). *)

module Make (K : Hashmap_intf.KEY) = struct

module AMR = Atomic_markable_ref

(** {Bit reversal and hashing} *)

(** [hash_key k] maps an arbitrary key to a non-negative hash in
    the range [0, max_int-1].  We use the user-supplied [K.hash]
    and further mask to [max_int - 1] to guarantee the result never
    equals [max_int] (which is reserved for the tail sentinel). *)
let hash_key k =
  (K.hash k) land (max_int - 1)

(** [reverse_bits x] reverses the bits of a non-negative integer.
    We reverse within [Sys.int_size - 1] useful bits (OCaml int is
    63 bits on 64-bit, sign bit excluded).  *)
let reverse_bits x =
  let num_bits = Sys.int_size - 1 in  (* useful bits in OCaml int *)
  let result = ref 0 in
  let x_ref = ref (x land max_int) in (* mask sign *)
  for _ = 0 to num_bits - 1 do
    result := (!result lsl 1) lor (!x_ref land 1);
    x_ref := !x_ref lsr 1
  done;
  !result

(** [make_regular_key hash] produces the split-ordered key for a regular
    (data) node by reversing the bits of the hashed value and then
    forcing the LSB to 1.

    This LSB=1 convention distinguishes regular nodes from sentinel nodes,
    whose split-ordered keys always have LSB=0. Because the global list is
    sorted by split-ordered key, a regular node will always appear *after*
    its bucket's sentinel in the list — they share the same upper bits, but
    1 > 0 in the LSB position. *)
let make_regular_key hashed =
  (reverse_bits hashed) lor 1

(** [make_sentinel_key bucket_idx] produces the split-ordered key for a
    sentinel node.  The LSB is forced to 0, which distinguishes sentinels
    from regular nodes (whose LSB is always 1). *)
let make_sentinel_key bucket_idx =
  (reverse_bits bucket_idx) land (lnot 1)

(** {Node types} *)

(** A node in the split-ordered list. *)
type 'v node = {
  so_key : int;                       (** split-ordered key *)
  real_key : K.t;                     (** original unhashed key *)
  value : 'v option Atomic.t;         (** atomic value — [None] for sentinel nodes,
                                          [Some v] for regular nodes.  Using Atomic.t
                                          allows in-place updates without remove+reinsert. *)
  next : 'v node AMR.t;              (** markable next pointer *)
}

(** Dummy sentinel key — used for sentinel nodes and the tail.
    Sentinels are distinguished by their so_key (LSB=0).  The
    [node_less_than] and [node_equal_key] functions guard against
    calling K.compare on sentinel keys — they only invoke K.compare
    when both so_keys are regular (LSB=1). *)
let sentinel_dummy_key : K.t = Obj.magic 0

(** Dummy terminal node — acts as list tail. *)
let make_tail () : 'v node =
  let dummy_next = AMR.create (Obj.magic ()) false in
  let tail = {
    so_key = max_int;
    real_key = sentinel_dummy_key;
    value = Atomic.make None;
    next = dummy_next;
  } in
  let _ = AMR.compare_and_set dummy_next
    ~expected_ref:(Obj.magic ()) ~new_ref:tail
    ~expected_mark:false ~new_mark:false
  in
  tail

(** {Table type} *)

type 'v t = {
  head : 'v node;                     (** list head sentinel (so_key = 0) *)
  buckets : 'v node option Atomic.t array;
                                      (** bucket → sentinel pointer *)
  bucket_count : int Atomic.t;        (** logical table size (power of 2) *)
  max_buckets : int;                  (** physical array length *)
  size : int Atomic.t;                (** number of regular nodes *)
  load_factor_threshold : float;
}

(** {Lock-free list operations}

    The list is sorted by [(so_key, real_key)] lexicographically.
    This secondary sort by [real_key] is necessary because [hash_key]
    can produce collisions — two different keys may share the same
    [so_key].  Without the secondary sort, a colliding key would be
    silently dropped, returned with the wrong value, or delete the
    wrong entry.

    Sentinel nodes use [sentinel_dummy_key] and have LSB=0 in their
    [so_key], so they occupy a different sort-key space from regular
    nodes (LSB=1) and don't interfere. *)

(** [is_regular_key so_key] returns [true] if [so_key] belongs to a
    regular (non-sentinel) node, i.e. its LSB is 1. *)
let is_regular so_key = so_key land 1 = 1

(** [node_less_than so rk curr] returns [true] if [curr] is ordered
    strictly before [(so, rk)] in the composite key order.
    K.compare is only invoked when both nodes are regular (LSB=1),
    so sentinel_dummy_key is never passed to K.compare. *)
let node_less_than so rk curr =
  if curr.so_key <> so then curr.so_key < so
  else if is_regular so then K.compare curr.real_key rk < 0
  else false  (* two sentinels with same so_key — equal, not less *)

(** [node_equal_key so rk curr] returns [true] if [curr] has the same
    composite key [(so, rk)].
    K.compare is only invoked when both nodes are regular. *)
let node_equal_key so rk curr =
  curr.so_key = so && (not (is_regular so) || K.compare curr.real_key rk = 0)

(** [locate head so_key real_key] finds the window [(pred, curr)] such
    that [pred] is strictly before [(so_key, real_key)] and [curr] is
    at-or-after, while physically removing marked nodes along the way. *)
let locate head so_key real_key =
  let is_marked = ref false in
  let rec retry pred =
    let curr = AMR.get_reference pred.next in
    let rec advance pred curr =
      let succ = AMR.get curr.next is_marked in
      if !is_marked then
        (* curr is logically deleted — try to snip it out *)
        if AMR.compare_and_set pred.next
            ~expected_ref:curr ~new_ref:succ
            ~expected_mark:false ~new_mark:false
        then advance pred succ
        else retry head                (* CAS failed, restart *)
      else if node_less_than so_key real_key curr then
        advance curr succ
      else
        (pred, curr)
    in
    advance pred curr
  in
  retry head

(** [list_insert head node] inserts [node] into the sorted list.
    Returns [true] if inserted, [false] if a node with the same
    [(so_key, real_key)] already exists. *)
let list_insert head node =
  let rec attempt () =
    let (pred, curr) = locate head node.so_key node.real_key in
    if node_equal_key node.so_key node.real_key curr then
      false   (* already present *)
    else begin
      (* point new node's next to curr *)
      let _ = AMR.compare_and_set node.next
        ~expected_ref:(AMR.get_reference node.next)
        ~new_ref:curr
        ~expected_mark:false ~new_mark:false in
      if AMR.compare_and_set pred.next
        ~expected_ref:curr ~new_ref:node
        ~expected_mark:false ~new_mark:false
      then true
      else attempt ()
    end
  in
  attempt ()

(** [list_find head so_key real_key] returns the node with matching
    [(so_key, real_key)] if present and not marked, or [None]. *)
let list_find head so_key real_key =
  let is_marked = ref false in
  let rec loop curr =
    let succ = AMR.get curr.next is_marked in
    if !is_marked then
      loop succ
    else if node_less_than so_key real_key curr then
      loop succ
    else if node_equal_key so_key real_key curr then
      Some curr
    else
      None
  in
  loop (AMR.get_reference head.next)

(** [list_remove head so_key real_key] logically removes the node with
    matching [(so_key, real_key)].  Returns [true] if found and removed. *)
let list_remove head so_key real_key =
  let rec attempt () =
    let (pred, curr) = locate head so_key real_key in
    if not (node_equal_key so_key real_key curr) then false
    else
      let succ = AMR.get_reference curr.next in
      (* logical delete: mark curr *)
      if AMR.compare_and_set curr.next
        ~expected_ref:succ ~new_ref:succ
        ~expected_mark:false ~new_mark:true
      then begin
        (* best-effort physical remove *)
        ignore (AMR.compare_and_set pred.next
          ~expected_ref:curr ~new_ref:succ
          ~expected_mark:false ~new_mark:false);
        true
      end
      else attempt ()
  in
  attempt ()

(** [get_parent idx] returns the parent bucket of [idx].
    The parent is [idx] with its highest set bit cleared.
    For example: parent(5) = parent(0b101) = 0b001 = 1
                 parent(6) = parent(0b110) = 0b010 = 2
                 parent(1) = parent(0b001) = 0b000 = 0 *)
let get_parent idx =
  if idx <= 0 then 0
  else begin
    (* Find the highest set bit position *)
    let msb = ref 1 in
    while !msb * 2 <= idx do
      msb := !msb * 2
    done;
    idx lxor !msb
  end

(** [init_bucket t idx] lazily initialises the sentinel for bucket [idx].
    If the parent bucket is not yet initialised, initialises it first
    (recursively).

    [get_bucket t idx] returns the sentinel node for bucket [idx],
    initialising it lazily if needed.

    These two functions are mutually recursive: [init_bucket] calls
    [get_bucket] to obtain the parent sentinel as the starting point
    for insertion (avoiding a full traversal from the head). *)
let rec init_bucket t idx =
  if idx >= t.max_buckets then ()
  else if idx = 0 then
    (* bucket 0 is always the head sentinel *)
    (match Atomic.get t.buckets.(0) with
     | Some _ -> ()
     | None -> Atomic.set t.buckets.(0) (Some t.head))
  else begin
    let parent = get_parent idx in
    (* ensure parent is initialised *)
    (match Atomic.get t.buckets.(parent) with
     | None -> init_bucket t parent
     | Some _ -> ());
    let parent_sentinel = get_bucket t parent in
    (* create sentinel node for this bucket *)
    let sentinel_key = make_sentinel_key idx in
    let tail = make_tail () in
    let sentinel = {
      so_key = sentinel_key;
      real_key = sentinel_dummy_key;
      value = Atomic.make None;
      next = AMR.create tail false;
    } in
    (* insert sentinel starting from parent (avoids traversing entire list) *)
    let _inserted = list_insert parent_sentinel sentinel in
    (* even if another thread already inserted it, find it.
       Sentinels use sentinel_dummy_key and are unique by so_key. *)
    match list_find parent_sentinel sentinel_key sentinel_dummy_key with
    | Some node -> Atomic.set t.buckets.(idx) (Some node)
    | None -> ()  (* should not happen *)
  end

and get_bucket t idx =
  match Atomic.get t.buckets.(idx) with
  | Some s -> s
  | None ->
    init_bucket t idx;
    (match Atomic.get t.buckets.(idx) with
     | Some s -> s
     | None -> t.head)   (* fallback *)

(** {Creation} *)

let create ?(capacity = 16) ?(load_factor = 2.0) () =
  if capacity <= 0 then
    invalid_arg "Split_ordered_hashmap.create: capacity must be > 0";
  (* round up capacity to a power of 2 *)
  let rec next_pow2 n = if n >= capacity then n else next_pow2 (n * 2) in
  let cap = next_pow2 1 in
  (* max_buckets is a large upper bound for the bucket array *)
  let max_buckets = cap * 256 in     (* allow up to 8 doublings *)
  let tail = make_tail () in
  let head = {
    so_key = 0;    (* sentinel for bucket 0 *)
    real_key = sentinel_dummy_key;
    value = Atomic.make None;
    next = AMR.create tail false;
  } in
  let buckets = Array.init max_buckets (fun _ -> Atomic.make None) in
  Atomic.set buckets.(0) (Some head);
  {
    head;
    buckets;
    bucket_count = Atomic.make cap;
    max_buckets;
    size = Atomic.make 0;
    load_factor_threshold = load_factor;
  }

(** {Resize (incremental)} *)

(** [maybe_resize t] doubles [bucket_count] if the load factor is exceeded.
    This is lock-free: we just CAS the counter.  New sentinel nodes will
    be initialised lazily by future operations. *)
let maybe_resize t =
  let sz = Atomic.get t.size in
  let bc = Atomic.get t.bucket_count in
  if float_of_int sz /. float_of_int bc > t.load_factor_threshold then begin
    let new_bc = bc * 2 in
    if new_bc <= t.max_buckets then
      ignore (Atomic.compare_and_set t.bucket_count bc new_bc)
    (* if CAS fails another thread already resized — that's fine *)
  end

(** {Operations} *)

let insert t key value =
  let h = hash_key key in
  let so_key = make_regular_key h in
  let tail = make_tail () in          (* allocate once, reused across retries *)
  let rec attempt () =
    let bc = Atomic.get t.bucket_count in   (* fresh read each retry *)
    let bucket_idx = h mod bc in
    let sentinel = get_bucket t bucket_idx in
    let node = {
      so_key;
      real_key = key;
      value = Atomic.make (Some value);
      next = AMR.create tail false;
    } in
    if list_insert sentinel node then begin
      Atomic.incr t.size;
      maybe_resize t
    end else begin
      (* key already exists — atomically update value in place *)
      match list_find sentinel so_key key with
      | Some existing -> Atomic.set existing.value (Some value)
      | None ->
        (* node was concurrently removed — retry *)
        attempt ()
    end
  in
  attempt ()

let find t key =
  let h = hash_key key in
  let so_key = make_regular_key h in
  let bc = Atomic.get t.bucket_count in
  let bucket_idx = h mod bc in
  let sentinel = get_bucket t bucket_idx in
  match list_find sentinel so_key key with
  | Some node -> Atomic.get node.value
  | None -> None

let remove t key =
  let h = hash_key key in
  let so_key = make_regular_key h in
  let bc = Atomic.get t.bucket_count in
  let bucket_idx = h mod bc in
  let sentinel = get_bucket t bucket_idx in
  if list_remove sentinel so_key key then begin
    Atomic.decr t.size;
    true
  end else
    false

let size t = Atomic.get t.size

end

(* Example instantiations:

   module IntMap = Make(struct
     type t = int
     let hash k  = (Hashtbl.hash k) land (max_int - 1)
     let equal   = Int.equal
     let compare = Int.compare
   end)

   module StringMap = Make(struct
     type t = string
     let hash    = Hashtbl.hash
     let equal   = String.equal
     let compare = String.compare
   end)

   module FloatMap = Make(struct
     type t = float
     let hash    = Hashtbl.hash
     let equal   = Float.equal
     let compare = Float.compare
   end) *)
