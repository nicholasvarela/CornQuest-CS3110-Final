val create_player : string -> Tsdl.Sdl.renderer -> Components.CornECS.entity
(**[create_player fl ren] creates a player entity with textures loaded from the
   file [fl] using the renderer [ren].*)

val draw : Components.CornECS.entity -> unit
(**[draw e] draws the entity [e] onscreen.*)

val handle : Components.CornECS.entity -> unit
(**[handle e] handles key state changes and their effects on the
   keyboard-controlled entity [e]'s movement.*)

val update : Components.CornECS.entity -> unit
(**[update e] updates the keyboard-controlled entity [e].*)
val steps : int ref
