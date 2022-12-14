(** Representation of character in CornQuest with skills, consumables and temp
    powers. *)

exception UnknownAttribute
(**Raised when an unknown attribute name is encountered.*)

exception No_skill
(**Raised when trying to access the skill fields of None*)

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

type dmg_type =
  | Magic
  | Physical
  | Status

type skill = {
  name : string;
  description : string;
  skill_type : dmg_type;
  attribute_affected : (attribute * int) array;
  chance_to_affect : float;
  base_dmg : float;
  dmg_scaling : float;
  mp_cost : float;
  hp_cost : float;
}
(** The abstract type representing a skill.*)

type consumable

type consumable_bucket = {
  name : string;
  item : consumable;
  amt : int;
}

type character = {
  name : string;
  hp : attribute;
  maxhp : attribute;
  mana : attribute;
  maxmana : attribute;
  exp : int;
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
  inv : consumable_bucket array;
  temp_stats : (attribute * int) array;
}

(**The abstract type representing a character.*)

val unwrap_skill : skill option -> skill
(**[unwrap_skill op] is the [sk] such that [op = Some sk]. Raises No_skill if
   [op = None] *)

val use_skill : skill -> character -> character -> character * character * bool
(**[use_skill sk user target] return the [user] and [target] characters after
   [user] uses [sk] on [target]. [Magic] and [Status] skills cost [Mana],
   [Physical] skills cost [hp]. Damage is calculated based on [target]'s
   [Defense] or [MagicResist] and [user]'s [Strength] or [MagicPower]. [sk] has
   a chance to hit calculated on [target]'s [Speed] and [user]'s [Accuracy], and
   a chance to apply status effects based on [sk.chance_to_affect]*)

val use_consumable : consumable -> character -> int -> character * bool
(**[use_consumable consumable char int] returns a character with [int] number of
   [consumeable] used *)

val get_enem_move_chance : character -> float list
(**[get_enem_move_chance enem] returns a list with [enem]'s percentage chances
   to use moves*)

val get_attribute_val : string -> character -> float
(**[get_attribute attr character] returns the value of the attribute
   corresponding to [attr]*)

val get_total_attr_val : string -> character -> float
(**[get_attribute attr character] returns the total value of the attribute
   corresponding to [attr], which is calculated by the temporary value +. base
   value*)

val get_skills : character -> skill option array
(**[get_skills actor] returns an array containing the skills of [actor]*)

val get_description_skill : skill -> string
(**[get_description_skill skl] returns the descriptions string of skill skl *)

val get_description_item : consumable -> string
(**[get_description_item csm] returns the descriptions string of consumable csm *)

val get_inv : character -> consumable_bucket array
(**[get_skills actor] returns the consumable inventory of [actor]*)

val clear_temps : character ref -> character
(**[clear_temps ch] decrements the amount of turns left for any applied
   temporary stat buffs or debuffs on [ch]. If a buff/debuff's turn counter
   reaches 0, the buff/debuff is removed from [ch].*)

val adjust_temps : attribute * int -> character -> character
(**[adjust temps (att, val) -> character -> character] returns character witth
   [attribute] adjusted by [val]*)

val get_name : character -> string
(**[get_name] returns the name property [character]*)

val get_temp_value : string -> character -> float
(**[get_temp_value] returns the temp value*)

val start_character : string -> character
(**[start_character name] is a level 1 default starting character named [name]
   with initialized attributes.*)

val parse_character : string -> float list -> character
(**[parse_character nme lst] parses a new character with default temp values
   from the json located at data/characters.json *)

val change_temps_from_skill : skill -> character -> character
(**[change_temps_from_skill] returns a character with temporary modified by
   skill*)

val adjust : float -> character -> string -> character
(**[adjust amt ch att] is the character [ch] with the attribute named [att]
   adjusted by [amt]. Negative values reduce said attribute. Raises
   [UnknownAttribute] if [att] is not a valid attribute name.*)

val level_up : character -> character
(**[level_up ch] is [ch] leveled up by 1.*)

val get_temp_value : string -> character -> float
(**[get_temp_value attr_st chr] is the float value of the temporary attribute of
   [chr] assosciated with [attr_str]*)

(**[print_skills actor] prints a list of [actor]'s skills into the console.*)

val icicle : skill
(**[icile] is a skill*)

val acid_spray : skill
(**[acid spray] is a skill*)

val icicle : skill
val acid_spray : skill
val blood : skill
val minimize : skill
val spin_slash : skill
val double_slash : skill
val headbutt : skill
val tsu : skill
val chainlight : skill
val piercing_light : skill
val nosferatu : skill

val parse_character : string -> float list -> character
(**[parse_character nme hit_chances] creates a new character with data from
   character.json with matching name = nme*)
