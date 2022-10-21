exception UnknownAttribute
(**Raised when an unknown attribute name is encountered.*)

(**The type representing an attribute of a character.*)
type attribute =
  | HP of float
  | Mana of float
  | Strength of float
  | Defense of float
  | MagicResist of float
  | Speed of float
  | Accuracy of float
  | MagicPower of float
  | Luck of float

type character
(**The abstract type representing a character.*)

type skill
(**The abstract type representing a skill.*)

val use_skill : skill -> character -> character -> character * character
val get_enem_move_chance : character -> float list
val get_attribute : attribute -> character -> float
val get_skills : character -> skill list
val clear_temps : character -> character
val adjust_temps : attribute -> character -> character

val start_character : string -> character
(**[start_character name] is a level 1 default starting character named [name]
   with initialized attributes.*)

val adjust : float -> character -> string -> character
(**[adjust amt ch att] is the character [ch] with the attribute named [att]
   adjusted by [amt]. Negative values reduce said attribute. Raises
   [UnknownAttribute] if [att] is not a valid attribute name.*)

val level_up : character -> character
(**[level_up ch] is [ch] leveled up by 1.*)
