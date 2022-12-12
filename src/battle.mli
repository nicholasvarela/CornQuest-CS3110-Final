(** Representation of text based battle simulator. *)

type action =
  | Attack of string
  | Guard
  | Cast of (string * string)
  | Flee

val attack : Character.character -> Character.character -> Character.character
(**[attack actor -> enem ] returns [actor] and [enem] after [actor] attacks
   [enem]*)

val enem_guard : Character.character -> Character.character
<<<<<<< HEAD
(**[enem_guard actor ] returns [actor] and [enem] after [actor] attacks [enem]*)
=======
(**[enem_guard actor -> enem ] returns [actor] and [enem] after [actor] attacks
   [enem]*)
>>>>>>> 1c5c84ee7929dc9189fc904decb2b026f468f68d

val guard : Character.character -> Character.character
(**[guard actor -> enem ] returns [actor] and [enem] after [actor] returns
   [enem]*)

val pick_enemy_move :
  Character.character * Character.character ->
  Character.character * Character.character
(**[pick_enemy_move (actor, enemy) ] returns [(actor' * enem')] with enemy used
   a move*)
