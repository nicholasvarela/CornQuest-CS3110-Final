type character
(**The abstract type representing a character.*)

exception UnknownAttribute
(**Raised when an unknown attribute name is encountered.*)

(**The type representing an attribute of a character.*)
type attribute =
  | HP of int
  | MP of int
  | Strength of int
  | Defense of int
  | Spirit of int
  | Speed of int
  | Accuracy of int
  | MagicPower of int
  | Luck of int

val start_character : string -> character
(**[start_character name] is a level 1 default starting character named [name]
   with initialized attributes.*)

val adjust : int -> character -> string -> character
(**[adjust amt ch att] is the character [ch] with the attribute named [att]
   adjusted by [amt]. Negative values reduce said attribute. Raises
   [UnknownAttribute] if [att] is not a valid attribute name.*)

val level_up : character -> character
(**[level_up ch] is [ch] leveled up by 1.*)
