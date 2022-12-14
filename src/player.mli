(**Module handling player-related functionality in the GUI.

   This module manages the creation and updating of players (of which there
   should ever only be one, in accordance with singleton pattern.)*)

exception FaultyImage
(**Exception raised when the dimensions of a supplied spritesheet are not
   divisors of [Constants.spritesize].*)

type t = Components.CornECS.entity
(**The abstract type of values representing players.*)

val create_player : string -> Tsdl.Sdl.renderer -> t
(**[create_player fl ren] creates a player with textures loaded from the
   spritesheet file [fl] using the renderer [ren].*)

val draw : t -> Camera.t -> unit
(**[draw e cam] draws the player [e] at the center of the camera [cam].*)

val handle : t -> unit
(**[handle e] handles key state changes and their effects on the
   keyboard-controlled player [e]'s movement.*)

val update : t -> Tilemap.t -> unit
(**[update e m cam] updates the keyboard-controlled player [e] on the map [m],
   with the camera [cam] centered on [e].*)

val set_spawn : t -> Tilemap.t -> unit
(**[set_spawn player m] sets the [player]'s spawn point on map [m].*)

val init_anims :
  t ->
  n:Tsdl.Sdl.texture ->
  e:Tsdl.Sdl.texture ->
  s:Tsdl.Sdl.texture ->
  w:Tsdl.Sdl.texture ->
  unit
(**[init_anims player n e s w] initializes the [player]'s animations such that
   the spritesheet [n] is associated with the direction [North], the spritesheet
   [e] is associated with the direction [East]..., and so on.*)
