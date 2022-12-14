module type ENTITY = sig
  type t

  val zero : t
  val incr : t ref -> unit
end

module IntEnt : ENTITY = struct
  type t = int

  let zero = 0
  let incr = incr
end

module type S = sig
  exception Property_error of string
  exception Empty_components

  type entity

  module type BASEPROP = sig
    val bind : entity -> unit
    val remove : entity -> unit
  end

  module type PROPERTY = sig
    type t

    val bind : entity -> unit
    val get : entity -> t
    val set : entity -> t -> unit
    val s : t -> entity -> entity
    val remove : entity -> unit
  end

  module type COMPONENT = sig
    val bind : entity -> unit
    val b : entity -> entity
    val remove : entity -> unit
    val bound : entity -> bool
    val iter : (entity -> unit) -> unit
  end

  val next_id : unit -> entity
  val delete : entity -> unit

  val new_component :
    (module BASEPROP) list -> (module COMPONENT) list -> (module COMPONENT)

  val new_property : ?default:'a -> unit -> (module PROPERTY with type t = 'a)
  val iter : (entity -> unit) -> entity list -> (module COMPONENT) list -> unit
  val iter_all : (entity -> unit) -> (module COMPONENT) list -> unit
  val filter : entity list -> (module COMPONENT) list -> entity list
  val filter_all : (module COMPONENT) list -> entity list
end

module Make (K : ENTITY) : S = struct
  exception Property_error of string
  exception Empty_components

  type entity = K.t

  module type BASEPROP = sig
    val bind : entity -> unit
    val remove : entity -> unit
  end

  module type PROPERTY = sig
    type t

    val bind : entity -> unit
    val get : entity -> t
    val set : entity -> t -> unit
    val s : t -> entity -> entity
    val remove : entity -> unit
  end

  module type COMPONENT = sig
    val bind : entity -> unit
    val b : entity -> entity
    val remove : entity -> unit
    val bound : entity -> bool
    val iter : (entity -> unit) -> unit
  end

  let last_id = ref K.zero

  let next_id () =
    K.incr last_id;
    !last_id

  (**Components are stored in a ref cell for ease of deletion.*)
  let components : (module COMPONENT) list ref = ref []

  let delete ent =
    List.iter (fun (module C : COMPONENT) -> C.remove ent) !components

  (**[register comp] registers the component [comp] to the system-wide list of
     components.*)
  let register comp = components := comp :: !components

  let new_component ps ds =
    let entities = ref [] in
    let module T = struct
      let bound e =
        let rec bound_aux = function
          | [] -> false
          | t :: q when t = e -> true
          | t :: q -> bound_aux q
        in
        bound_aux !entities

      let bind e =
        if bound e then () else entities := e :: !entities;
        List.iter (fun (module P : BASEPROP) -> P.bind e) ps;
        List.iter (fun (module C : COMPONENT) -> C.bind e) ds

      let b e =
        bind e;
        e

      let remove e =
        List.iter (fun (module P : BASEPROP) -> P.remove e) ps;
        let rec remove_aux = function
          | [] -> []
          | t :: q when t = e -> q
          | t :: q -> t :: remove_aux q
        in
        entities := remove_aux !entities

      let iter f = List.iter f !entities
    end in
    register (module T : COMPONENT);
    (module T : COMPONENT)

  let new_property (type s) ?default () =
    let table = Hashtbl.create 10 in
    let module T = struct
      type t = s

      let bind e =
        try ignore (Hashtbl.find table e)
        with Not_found -> Hashtbl.add table e default

      let get e =
        try
          match Hashtbl.find table e with
          | None ->
              raise
                (Property_error
                   "No default value was provided for the property.")
          | Some v -> v
        with Not_found -> raise (Property_error "Property not bound.")

      let set e v = Hashtbl.replace table e (Some v)

      let s v e =
        set e v;
        e

      let remove = Hashtbl.remove table
    end in
    (module T : PROPERTY with type t = s)

  let iter f es cs =
    let rec cfilter e =
      if List.fold_left (fun b (module C : COMPONENT) -> b && C.bound e) true cs
      then f e
    in
    List.iter cfilter es

  let iter_all f = function
    | [] -> raise Empty_components
    | (module T : COMPONENT) :: q ->
        let rec cfilter e =
          if
            List.fold_left
              (fun b (module C : COMPONENT) -> b && C.bound e)
              true q
          then f e
        in
        T.iter cfilter

  let filter es cs =
    let l = ref [] in
    iter (fun e -> l := e :: !l) es cs;
    !l

  let filter_all cs =
    let l = ref [] in
    iter_all (fun e -> l := e :: !l) cs;
    !l
end