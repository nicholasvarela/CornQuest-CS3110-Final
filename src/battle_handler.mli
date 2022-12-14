(** Central battle engine handler for CornQuest*)

exception Battle_Over of Character.character option

val wait : unit -> unit
(**[wait ()] causes a pause in the game*)

val start : Character.character -> Character.character -> 'a
(** [start actor -> enem] initializes the battle engine with [actor] as the
    player and [enem] as the enemy*)

val read_logo_files : string -> unit
(** [read_logo_files filename] prints the string contained in [filename] on the
    terminal which has the battle engine initialized*)
