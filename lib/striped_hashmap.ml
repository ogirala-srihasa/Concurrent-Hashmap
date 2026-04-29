(** Striped Concurrent Hash Map

    Uses an array of mutex-protected bucket stripes.  Each stripe is a
    [Mutex.t] that guards a subset of buckets (bucket_index mod
    num_stripes).  Supports dynamic resizing: when the load factor exceeds a
    configurable threshold, all stripes are acquired in order, the bucket
    array is doubled, and every entry is rehashed.

    Based on AoMPP Chapter 13 and Java's ConcurrentHashMap (pre-JDK-8). *)

module Make (K : Hashmap_intf.KEY) = struct

  (** {Types} *)

  type 'v t = {
    mutable buckets : (K.t * 'v) list array;
    locks : Mutex.t array;
    num_stripes : int;
    size : int Atomic.t;
    mutable capacity : int;              (* current number of buckets *)
    load_factor_threshold : float;
    resize_lock : Mutex.t;               (* serialises resize attempts *)
  }

  (** { Helpers} *)

  let stripe_index t bucket_idx =
    bucket_idx mod t.num_stripes

  let bucket_for_key t key =
    (* ensure non-negative: mask off sign bit of the hash *)
    (K.hash key land max_int) mod t.capacity

  (** { Creation} *)

  let create ?(capacity = 16) ?(num_stripes = 16) ?(load_factor = 0.75) () =
    if capacity <= 0 then invalid_arg "Striped_hashmap.create: capacity must be > 0";
    if num_stripes <= 0 then invalid_arg "Striped_hashmap.create: num_stripes must be > 0";
    let effective_capacity = max capacity num_stripes in
    {
      buckets = Array.make effective_capacity [];
      locks = Array.init num_stripes (fun _ -> Mutex.create ());
      num_stripes;
      size = Atomic.make 0;
      capacity = effective_capacity;
      load_factor_threshold = load_factor;
      resize_lock = Mutex.create ();
    }

  (** { Resizing} *)

  (** [should_resize t] returns [true] when the load factor is exceeded. *)
  let should_resize t =
    let load = float_of_int (Atomic.get t.size) /. float_of_int t.capacity in
    load > t.load_factor_threshold

  (** [resize t] doubles the bucket array.  Must be called WITHOUT holding any
      stripe lock.  We acquire the global resize_lock, re-check the condition,
      then acquire every stripe lock in order before rehashing. *)
  let resize t =
    Mutex.lock t.resize_lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock t.resize_lock) (fun () ->
      (* double-check after acquiring resize_lock *)
      if not (should_resize t) then ()
      else begin
        (* acquire all stripe locks in order to prevent deadlock *)
        for i = 0 to t.num_stripes - 1 do
          Mutex.lock t.locks.(i)
        done;
        Fun.protect ~finally:(fun () ->
          for i = t.num_stripes - 1 downto 0 do
            Mutex.unlock t.locks.(i)
          done)
        (fun () ->
          let old_cap = t.capacity in
          let new_cap = old_cap * 2 in
          let new_buckets = Array.make new_cap [] in
          (* rehash every entry *)
          Array.iter (fun chain ->
            List.iter (fun ((k, _v) as entry) ->
              let idx = (K.hash k land max_int) mod new_cap in
              new_buckets.(idx) <- entry :: new_buckets.(idx))
            chain)
          t.buckets;
          t.buckets <- new_buckets;
          t.capacity <- new_cap)
      end)

  (** {Operations}

      Each operation computes the bucket index, acquires the stripe lock,
      then re-validates the bucket index inside the lock.  If a concurrent
      resize changed the capacity between the initial read and the lock
      acquisition, the bucket index may have moved to a different stripe,
      so we release and retry. *)

  let insert t key value =
    let rec attempt () =
      let bi = bucket_for_key t key in
      let si = stripe_index t bi in
      Mutex.lock t.locks.(si);
      let bi' = bucket_for_key t key in
      if bi <> bi' then begin
        (* resize happened — bucket moved to a different stripe *)
        Mutex.unlock t.locks.(si);
        attempt ()
      end else
        Fun.protect ~finally:(fun () -> Mutex.unlock t.locks.(si)) (fun () ->
          let bucket = t.buckets.(bi) in
          let rec replace = function
            | [] ->
              (* key not found — prepend *)
              t.buckets.(bi) <- (key, value) :: bucket;
              Atomic.incr t.size
            | (k, _v) :: _rest when K.equal k key ->
              (* key exists — update in place by rebuilding the chain *)
              t.buckets.(bi) <-
                List.map (fun (k', v') -> if K.equal k' key then (k', value) else (k', v')) bucket
            | _ :: rest -> replace rest
          in
          replace bucket)
    in
    attempt ();
    (* resize outside the stripe lock *)
    if should_resize t then resize t

  let find t key =
    let rec attempt () =
      let bi = bucket_for_key t key in
      let si = stripe_index t bi in
      Mutex.lock t.locks.(si);
      let bi' = bucket_for_key t key in
      if bi <> bi' then begin
        Mutex.unlock t.locks.(si);
        attempt ()
      end else
        Fun.protect ~finally:(fun () -> Mutex.unlock t.locks.(si)) (fun () ->
          let rec assoc_opt = function
            | [] -> None
            | (k, v) :: _ when K.equal k key -> Some v
            | _ :: rest -> assoc_opt rest
          in
          assoc_opt t.buckets.(bi))
    in
    attempt ()

  let remove t key =
    let rec attempt () =
      let bi = bucket_for_key t key in
      let si = stripe_index t bi in
      Mutex.lock t.locks.(si);
      let bi' = bucket_for_key t key in
      if bi <> bi' then begin
        Mutex.unlock t.locks.(si);
        attempt ()
      end else
        Fun.protect ~finally:(fun () -> Mutex.unlock t.locks.(si)) (fun () ->
          let bucket = t.buckets.(bi) in
          let found = List.exists (fun (k, _) -> K.equal k key) bucket in
          if found then begin
            t.buckets.(bi) <- List.filter (fun (k, _) -> not (K.equal k key)) bucket;
            Atomic.decr t.size
          end;
          found)
    in
    attempt ()

  let size t = Atomic.get t.size

  let capacity t = t.capacity

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
