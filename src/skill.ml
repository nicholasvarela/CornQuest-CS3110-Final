type dmg_type =
  | Magic
  | Physical
  | Status

type skill = {
  name : string;
  dmg_type : dmg_type;
  attribute_affected : string;
  dmg : int;
  mp_cost : int;
  hp_cost : int;
}

(**[cost_calc sk user] is the character [user]'s remaining HP and MP reserves
   after deducting the costs of using [sk].*)
let cost_calc sk user =
  Character.adjust sk.hp_cost (Character.adjust sk.mp_cost user "mp") "hp"

let use_skill sk user target =
  match sk.dmg_type with
  | Magic ->
      (cost_calc sk user, Character.adjust sk.dmg target sk.attribute_affected)
  | Physical ->
      (cost_calc sk user, Character.adjust sk.dmg target sk.attribute_affected)
  | Status ->
      (cost_calc sk user, Character.adjust sk.dmg target sk.attribute_affected)
