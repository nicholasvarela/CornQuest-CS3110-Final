(**Module handling camera-related functionality in the GUI.

   This module manages the creation and updating of in-game cameras (of which
   there should ever only be one, in accordance with singleton pattern.)*)

type t
(**The abstract type representing cameras.*)

val create_camera : Components.CornECS.entity -> t
(**[create_camera e] creates a camera centered on the entity [e].*)

val track : t -> Components.CornECS.entity -> unit
(**[track cam e] makes the camera [cam] center on the entity [e].*)

val delete : t -> unit
(**[delete cam] deletes the camera [cam] from the game.*)

val get_pos : t -> int * int
(**[get_pos cam] gets the coordinates [cam] is currently following.*)