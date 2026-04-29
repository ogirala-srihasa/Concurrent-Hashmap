(** Split-Ordered List Hash Map — Lock-Free with Incremental Resizing

    Based on Shalev & Shavit 2006.  Keys are stored in bit-reversed order
    in a single lock-free linked list.  Bucket sentinels are lazily created,
    enabling lock-free incremental resizing. *)

module Make (K : Hashmap_intf.KEY) : sig
  type 'v t

  val create : ?capacity:int -> ?load_factor:float -> unit -> 'v t
  (** [create ~capacity ~load_factor ()] creates a new map.
      @param capacity   Initial bucket count (rounded up to power of 2, default 16)
      @param load_factor Resize threshold (default 2.0) *)

  val insert : 'v t -> K.t -> 'v -> unit
  (** [insert t key value] inserts or updates [key -> value]. *)

  val find : 'v t -> K.t -> 'v option
  (** [find t key] returns [Some v] if present, [None] otherwise. *)

  val remove : 'v t -> K.t -> bool
  (** [remove t key] removes the mapping.  Returns [true] if found. *)

  val size : 'v t -> int
  (** [size t] returns the current number of stored mappings. *)
end
