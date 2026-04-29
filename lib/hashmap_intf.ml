(** Common interface for concurrent hash maps.

    Both the striped and split-ordered implementations are parameterised
    by a [KEY] module type and satisfy the [HASHMAP] signature. *)

(** The type that key modules must satisfy. *)
module type KEY = sig
  type t
  val hash    : t -> int
  val equal   : t -> t -> bool
  val compare : t -> t -> int
end

module type HASHMAP = sig
  type key
  (** The type of keys in the map. *)

  type 'v t
  (** A concurrent hash map mapping keys to values of type ['v]. *)

  val create : ?capacity:int -> unit -> 'v t
  (** [create ~capacity ()] creates a new hash map.
      @param capacity Initial number of buckets (default: 16). Must be > 0.
      @raise Invalid_argument if capacity <= 0 *)

  val insert : 'v t -> key -> 'v -> unit
  (** [insert t key value] inserts [key -> value].
      If [key] already exists, its value is updated. *)

  val find : 'v t -> key -> 'v option
  (** [find t key] returns [Some v] if [key] maps to [v], [None] otherwise. *)

  val remove : 'v t -> key -> bool
  (** [remove t key] removes the mapping for [key].
      Returns [true] if [key] was present, [false] otherwise. *)

  val size : 'v t -> int
  (** [size t] returns the number of key-value pairs currently stored. *)
end
