(**Entity-component system.

   This module forms the basis of the game's architectural logic — a simple
   entity-component system. Entities are composed of components, which are in
   turn composed of basic properties (data). At the top level, entities are
   managed by a system that keeps track of which entities contain which
   components, and what properties each component contains.

   Example: To create a new system that uses integers as identifiers for
   entities, use the [ECS.Make] functor: [module NewECS = ECS.Make(ECS.IntEnt)].
   Entities, properties, and components can then be created under this system.*)

(** Input signature of the functor [Make]. *)
module type ENTITY = sig
  type t
  (**The abstract type of entities, which are in-game structures that contain
     "components", or collections of properties.*)

  val zero : t
  (**The representation of the zero-valued entity, or the "first" entity. *)

  val incr : t ref -> unit
  (**[incr id] is the identifier value that logically follows [id], depending on
     what the type [t] is.*)
end

module IntEnt : ENTITY
(** Implementation of an entity type using integers as identifiers. *)

(**Output signature of the functor [Make].*)
module type S = sig
  exception Property_error of string
  (**Exception raised by property accessors when the property is not bound or
     has no default value.*)

  exception Empty_components
  (**Exception raised by [iter_all] when there are no components to filter
     through.*)

  type entity
  (**Abstract type of entities.*)

  (** Non-type-dependent signature for properties; allows for properties to be
      made into lists. *)
  module type BASEPROP = sig
    val bind : entity -> unit
    (**TODO: is there no way to hide this function in the interface?

       [bind e] binds the property to the entity [e]. This is done automatically
       at the component level, which means that clients should have no reason to
       use the property-level [bind] function. *)

    val remove : entity -> unit
    (**[remove e] removes the property from [e].*)
  end

  (** Signature for properties, which carry basic data and are generated by
      [new_property] as first-class modules at the system level.*)
  module type PROPERTY = sig
    type t
    (**The abstract type of data contained in the property.*)

    val bind : entity -> unit
    (**TODO: is there no way to hide this function in the interface?

       [bind e] binds the property to the entity [e]. This is done automatically
       at the component level, which means that clients should have no reason to
       use the property-level [bind] function. *)

    val get : entity -> t
    (**[get e] is the current value of the property under the entity [e], if it
       has been bound to [e] and set to a value. Raises: [Property_error] if the
       property has not been set/initialized to a default value, or if the
       property has not been bound to [e].*)

    val set : entity -> t -> unit
    (**[set e v] sets the property under the entity [e] to the value [v].*)

    val s : t -> entity -> entity
    (** [s v e] has the same effects as [set e v], but has its arguments in
        reversed order and returns the modified entity [e] for ease of
        pipelining. *)

    val remove : entity -> unit
    (**[remove e] removes the property from [e].*)
  end

  (** Signature for components, which are groups of properties and are generated
      by [new_component] as first-class modules at the system level. *)
  module type COMPONENT = sig
    val bind : entity -> unit
    (**[bind e] binds the component to an entity [e], which also automatically
       binds the properties associated with the component to the entity [e].
       Values of properties must be initialized before use, or a default value
       must be provided, otherwise attempts to access the properties will raise
       [Property_error].*)

    val b : entity -> entity
    (**[b e] has the same effect as [bind e], but returns the modified [e] for
       ease of pipelining.*)

    val remove : entity -> unit
    (**[remove e] removes the component from [e].*)

    val bound : entity -> bool
    (**[bound e] is [true] if the component is bound to [e], and [false] if
       otherwise.*)

    val iter : (entity -> unit) -> unit
    (**[iter f] applies the function [f] to all entities that the component is
       currently bound to.*)
  end

  val next_id : unit -> entity
  (**[next_id ()] is the entity-id to be used by the next entity to be created,
     given by [K.incr]. *)

  val delete : entity -> unit
  (**[delete ent] deletes [ent], removing all components from it. *)

  val new_component :
    (module BASEPROP) list -> (module COMPONENT) list -> (module COMPONENT)
  (** [new_component ps ds] creates a component (as a first-class module)
      containing properties [ps] and depending on components [ds].*)

  val new_property : ?default:'a -> unit -> (module PROPERTY with type t = 'a)
  (** [new_property d] creates a property (as a first-class module) with default
      value [d]. When binding a component containing this property to an entity,
      [d] will be used as the initial value.*)

  val iter : (entity -> unit) -> entity list -> (module COMPONENT) list -> unit
  (** [iter f es cs] applies the function [f] to all the entities in [es] that
      are bound to all the components contained in [cs].*)

  val iter_all : (entity -> unit) -> (module COMPONENT) list -> unit
  (** [iter_all f cs] applies the function [f] to every entity that is bound to
      all the components contained in [cs]. Raises: [Empty_components] if [cs]
      is the the empty list.*)

  val filter : entity list -> (module COMPONENT) list -> entity list
  (** [filter es cs] returns the sublist of [es] containing only the entities
      that are bound to all components of [cs]. Allows for easy creation of
      Systems.*)

  val filter_all : (module COMPONENT) list -> entity list
  (** [filter_all cs] returns the list of all entities bound to all the
      components specified in [cs]. *)
end

(**Functor that builds an entity-component system given a type [K] to use as
   entity-id that defines its own incrementation.*)
module Make (K : ENTITY) : S
