(* type skill
(**The abstract type representing a skill.*)

val use_skill :
  skill ->
  Character.character ->
  Character.character ->
  Character.character * Character.character
(**[use_skill sk user target] is the tuple of characters [(user, target)] after
   [user] casts the skill [sk] on [target].*) *)
