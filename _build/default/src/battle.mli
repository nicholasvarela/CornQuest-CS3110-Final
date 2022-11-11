type party_state
(**The abstract type representing the state of the party during a battle.*)

type action =
  | Attack of string
  | Guard
  | Cast of (string * string)
  | Flee

val attack : Character.character -> Character.character -> Character.character
val enem_guard : Character.character -> Character.character
val guard : Character.character -> Character.character

val pick_enemy_move :
  Character.character * Character.character ->
  Character.character * Character.character
