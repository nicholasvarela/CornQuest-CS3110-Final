(* type attribute =
  | HP of int
  | MP of int
  | Strength of int
  | Defense of int
  | Spirit of int
  | Speed of int
  | Accuracy of int
  | MagicPower of int
  | Luck of int

type character = {cle
  name : string;
  hp : attribute;
  mp : attribute;
  exp : int;
  lvl : int;
  str : attribute;
  def : attribute;
  mdef : attribute;
  spd : attribute;
  acc : attribute;
  mag : attribute;
  luk : attribute;
}

exception UnknownAttribute

let start_character name =
  {
    name;
    hp = HP 100;
    mp = MP 100;
    exp = 0;
    lvl = 1;
    str = Strength 10;
    def = Defense 10;
    mdef = Spirit 10;
    spd = Speed 10;
    acc = Accuracy 10;
    mag = MagicPower 10;
    luk = Luck 10;
  }

let ( +* ) att amt =
  match att with
  | HP hp -> HP (hp + amt)
  | MP mp -> MP (mp + amt)
  | Strength str -> MP (str + amt)
  | Defense def -> MP (def + amt)
  | Spirit mdef -> MP (mdef + amt)
  | Speed spd -> MP (spd + amt)
  | Accuracy acc -> MP (acc + amt)
  | MagicPower mag -> MP (mag + amt)
  | Luck luk -> MP (luk + amt)

let adjust amt ch = function
  | "hp" -> { ch with hp = ch.hp +* amt }
  | "mp" -> { ch with mp = ch.mp +* amt }
  | "strength" -> { ch with str = ch.str +* amt }
  | "defense" -> { ch with def = ch.def +* amt }
  | "spirit" -> { ch with mdef = ch.mdef +* amt }
  | "speed" -> { ch with spd = ch.spd +* amt }
  | "accuracy" -> { ch with acc = ch.acc +* amt }
  | "magic power" -> { ch with mag = ch.mag +* amt }
  | "luck" -> { ch with luk = ch.luk +* amt }
  | _ -> raise UnknownAttribute

let level_up ch = { ch with exp = 0; lvl = ch.lvl + 1 } *)
