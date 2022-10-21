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
  skill_type : dmg_type;
  attribute_affected : string;
  dmg : float;
  mp_cost : float;
  hp_cost : float;
}

type character = {
  name : string;
  hp : attribute;
  mana : attribute;
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
  skillset : skill list;
  temp_stats : attribute list;
}

exception UnknownAttribute
exception WrongAttribute

let get_enem_move_chance actor = actor.enem_hit_chances
let get_skills actor = actor.skillset

let get_attribute attr character =
  match attr with
  | HP _ -> character.hp
  | Mana _ -> character.mana
  | Strength _ -> character.str
  | Defense _ -> character.def
  | MagicResist _ -> character.mr
  | Speed _ -> character.spd
  | Accuracy _ -> character.acc
  | MagicPower _ -> character.mag
  | Luck _ -> character.luk

let clear_temps character =
  {
    name = character.name;
    hp = character.hp;
    mana = character.mana;
    exp = character.exp;
    lvl = character.lvl;
    str = character.str;
    def = character.def;
    mr = character.mr;
    spd = character.spd;
    acc = character.acc;
    mag = character.mag;
    luk = character.luk;
    enem_hit_chances = character.enem_hit_chances;
    skillset = character.skillset;
    temp_stats = [];
  }

let adjust_temps (attr : attribute) character =
  {
    name = character.name;
    hp = character.hp;
    mana = character.mana;
    exp = character.exp;
    lvl = character.lvl;
    str = character.str;
    def = character.def;
    mr = character.mr;
    spd = character.spd;
    acc = character.acc;
    mag = character.mag;
    luk = character.luk;
    enem_hit_chances = character.enem_hit_chances;
    skillset = character.skillset;
    temp_stats = attr :: character.temp_stats;
  }

let start_character nme =
  {
    name = nme;
    hp = HP 100.;
    mana = Mana 100.;
    exp = 0.;
    lvl = 1;
    str = Strength 10.;
    def = Defense 10.;
    mr = MagicResist 10.;
    spd = Speed 10.;
    acc = Accuracy 10.;
    mag = MagicPower 10.;
    luk = Luck 10.;
    enem_hit_chances = [];
    skillset = [];
    temp_stats = [];
  }

let ( +* ) att amt =
  match att with
  | HP hp -> HP (hp +. amt)
  | Mana mp -> Mana (mp +. amt)
  | Strength str -> Mana (str +. amt)
  | Defense def -> Mana (def +. amt)
  | MagicResist mdef -> Mana (mdef +. amt)
  | Speed spd -> Mana (spd +. amt)
  | Accuracy acc -> Mana (acc +. amt)
  | MagicPower mag -> Mana (mag +. amt)
  | Luck luk -> Mana (luk +. amt)

let adjust amt ch = function
  | "hp" -> { ch with hp = ch.hp +* amt }
  | "mana" -> { ch with mana = ch.mana +* amt }
  | "strength" -> { ch with str = ch.str +* amt }
  | "defense" -> { ch with def = ch.def +* amt }
  | "spirit" -> { ch with mr = ch.mr +* amt }
  | "speed" -> { ch with spd = ch.spd +* amt }
  | "accuracy" -> { ch with acc = ch.acc +* amt }
  | "magic power" -> { ch with mag = ch.mag +* amt }
  | "luck" -> { ch with luk = ch.luk +* amt }
  | _ -> raise UnknownAttribute

let level_up ch = { ch with exp = 0.; lvl = ch.lvl + 1 }
let cost_calc sk user = adjust sk.hp_cost (adjust sk.mp_cost user "mp") "hp"

let use_skill sk user target =
  match sk.skill_type with
  | Magic -> (cost_calc sk user, adjust sk.dmg target sk.attribute_affected)
  | Physical -> (cost_calc sk user, adjust sk.dmg target sk.attribute_affected)
  | Status -> (cost_calc sk user, adjust sk.dmg target sk.attribute_affected)
