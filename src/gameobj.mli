(** Representation of a dynamic object in a game.

    This module represents an object belonging in a game instance, including its
    renderer and how it should update itself. *)

type t
(** The abstract type of values representing the an in-game object. *)

val create : string -> Tsdl.Sdl.renderer -> int -> int -> t
(**[create tx ren xpos ypos] is an object with renderer [ren], textures loaded
   from [tx], and initial position on the window [(xpos, ypos)].*)

val update : t -> int -> int -> unit
(**[update obj] updates [obj], moving its x position by [x_inc] and its y
   position by [y_inc].*)

val render : t -> Tsdl.Sdl.renderer -> unit Tsdl.Sdl.result
(**[render obj ren] renders the object [obj] onscreen with renderer [ren].*)
