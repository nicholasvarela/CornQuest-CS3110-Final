(** Representation of text based battle simulator. *)

type action =
  | Attack of string
  | Guard
  | Cast of (string * string)
  | Flee

val attack : Character.character -> Character.character -> Character.character
(**[attack actor -> enem ] returns [actor] and [enem] after [actor] attacks
   [enem]*)

val guard : Character.character -> Character.character
(**[guard actor -> enem ] returns [actor] and [enem] after [actor] returns
   [enem]*)

val pick_enemy_move :
  Character.character * Character.character ->
  Character.character * Character.character
(**[pick_enemy_move (actor, enemy) ] returns [(actor' * enem')] with enemy used
   a move*)
