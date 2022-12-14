(**Representation of an audio player.

   This module represents a simple audio system based on [Tsdl_mixer] that is
   designed to load and play music with minimal interfacing with the convoluted
   [Tsdl] library. There should only be one of these active in a game instance.*)

type t
(**The abstract type of an audio player.*)

val init : unit -> t
(**[init ()] initializes an audio player. Raises: [Invalid_argument] if the
   process fails.*)

val load_music : t -> string -> int -> unit
(**[load_music sys fl id] loads music from the file [fl] into [sys], assigning
   it an id of [id]. If [id] already corresponds to a music track, it is
   overwritten. Requires: [fl] is a .wav or .ogg file, and [sys] is an
   initialized audio system.*)

val close : t -> unit
(**[close sys] closes the audio system [sys].*)

val play : t -> int -> unit
(**[play sys id] plays the music stored in system [sys] with id [id], stopping
   any other music that happens to be playing. *)
