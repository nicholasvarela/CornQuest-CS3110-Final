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

type skill
(**The abstract type representing a skill.*)

type character = {
  name : string;
  hp : attribute;
  maxhp : attribute;
  mana : attribute;
  maxmana : attribute;
  exp : float;
  lvl : int;
  mr : attribute;
  str : attribute;
  def : attribute;
  spd : attribute;
  acc : attribute;
  mag : attribute;
  luk : attribute;
  enem_hit_chances : float list;
  skillset : skill option array;
  temp_stats : (attribute * int) array;
}
(**The abstract type representing a character.*)

val use_skill : skill -> character -> character -> character * character
val get_enem_move_chance : character -> float list
val get_attribute : string -> character -> float
val get_skills : character -> skill option array
val clear_temps : character -> character
val adjust_temps : attribute * int -> character -> character
val get_name : character -> string

val start_character : string -> character
(**[start_character name] is a level 1 default starting character named [name]
   with initialized attributes.*)

val adjust : float -> character -> string -> character
(**[adjust amt ch att] is the character [ch] with the attribute named [att]
   adjusted by [amt]. Negative values reduce said attribute. Raises
   [UnknownAttribute] if [att] is not a valid attribute name.*)

val level_up : character -> character
(**[level_up ch] is [ch] leveled up by 1.*)

val get_temps : character -> float
