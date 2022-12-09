(** Representation of tilemap data.

    This module represents the data stored in tilemap files. It handles loading
    of that data from JSON as well as querying the data.*)

type t
(**The abstract type representing tilemap data.*)

val load_map : string -> string -> Tsdl.Sdl.renderer -> t
(**[load_map tx fl ren] creates a new tilemap representation, with textures
   loaded from [tx] using renderer [ren], and map data from the JSON file [fl].
   Requires: [fl] is a well-formed JSON file. *)

val draw_map : t -> Tsdl.Sdl.renderer -> unit
(**[draw_map m ren] draws the tilemap [m] onscreen using renderer [ren]. *)