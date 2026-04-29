(** Striped Concurrent Hash Map

    Uses an array of mutex-protected bucket stripes.  Each stripe guards a
    contiguous set of buckets [(bucket_index mod num_stripes)].  Supports
    dynamic resizing when the load factor exceeds a threshold. *)

module Make (K : Hashmap_intf.KEY) : sig
  type 'v t

  val create : ?capacity:int -> ?num_stripes:int -> ?load_factor:float -> unit -> 'v t
  (** [create ~capacity ~num_stripes ~load_factor ()] creates a new map.
      @param capacity   Initial bucket count (default 16, must be > 0)
      @param num_stripes Number of mutex stripes (default 16, must be > 0)
      @param load_factor Resize threshold (default 0.75) *)

  val insert : 'v t -> K.t -> 'v -> unit
  (** [insert t key value] inserts or updates [key -> value]. *)

  val find : 'v t -> K.t -> 'v option
  (** [find t key] returns [Some v] if present, [None] otherwise. *)

  val remove : 'v t -> K.t -> bool
  (** [remove t key] removes the mapping.  Returns [true] if found. *)

  val size : 'v t -> int
  (** [size t] returns the current number of stored mappings. *)

  val capacity : 'v t -> int
  (** [capacity t] returns the current number of buckets. *)
end
