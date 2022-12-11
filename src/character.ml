exception No_skill

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
  attribute_affected : (attribute * int) array;
  chance_to_affect : float;
  base_dmg : float;
  dmg_scaling : float;
  mp_cost : float;
  hp_cost : float;
}

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

exception UnknownAttribute
exception WrongAttribute

(*skill repo start*)

let icicle : skill =
  {
    name = "icicle";
    skill_type = Magic;
    attribute_affected =
      [|
        (HP 0., -1);
        (Mana 0., -1);
        (Strength 0., -1);
        (Defense 0., -1);
        (MagicResist 0., -1);
        (Speed (-6.), 3);
        (Accuracy (-5.), 3);
        (MagicPower 0., -1);
        (Luck 0., -1);
      |];
    chance_to_affect = 0.3;
    base_dmg = 5.;
    dmg_scaling = 0.4;
    mp_cost = 15.;
    hp_cost = 0.;
  }

let piercing_light =
  {
    name = "piercing light";
    skill_type = Magic;
    attribute_affected =
      [|
        (HP 0., -1);
        (Mana 0., -1);
        (Strength 0., -1);
        (Defense 0., -1);
        (MagicResist (-2.), 3);
        (Speed 0., -1);
        (Accuracy 0., -1);
        (MagicPower 0., -1);
        (Luck 0., -1);
      |];
    chance_to_affect = 0.9;
    base_dmg = 2.;
    dmg_scaling = 0.16;
    mp_cost = 30.;
    hp_cost = 0.;
  }

(*skill repo end*)
let get_enem_move_chance enem = enem.enem_hit_chances
let get_skills actor = actor.skillset

let unwrap_skill sk =
  match sk with
  | None -> raise No_skill
  | Some s -> s

let unwrap_attr = function
  | HP h -> h
  | Mana h -> h
  | Strength h -> h
  | Defense h -> h
  | MagicResist h -> h
  | Speed h -> h
  | Accuracy h -> h
  | MagicPower h -> h
  | Luck h -> h

let change_temp_attr_overwrite amt = function
  | HP _ -> HP amt
  | Mana _ -> Mana amt
  | Strength _ -> Strength amt
  | Defense _ -> Defense amt
  | MagicResist _ -> MagicResist amt
  | Speed _ -> Speed amt
  | Accuracy _ -> Accuracy amt
  | MagicPower _ -> MagicPower amt
  | Luck _ -> Luck amt

let get_attribute attr character =
  match attr with
  | "maxhp" -> unwrap character.hp
  | "maxmana" -> unwrap character.mana
  | "hp" -> unwrap character.hp
  | "mana" -> unwrap character.mana
  | "strength" -> unwrap character.str
  | "defense" -> unwrap character.def
  | "magic resist" -> unwrap character.mr
  | "magic power" -> unwrap character.mag
  | "speed" -> unwrap character.spd
  | "accuracy" -> unwrap character.acc
  | "magic" -> unwrap character.mag
  | "luck" -> unwrap character.luk
  | _ -> failwith "not valid attr"

let clear_temps character =
  let arr = character.temp_stats in
  for i = 0 to Array.length arr - 1 do
    let at, n = arr.(i) in
    if n = 0 then arr.(i) <- (change_temp_attr_overwrite 0. at, -1)
    else if n > 0 then arr.(i) <- (at, n - 1)
  done;
  {
    name = character.name;
    hp = character.hp;
    maxhp = character.maxhp;
    mana = character.mana;
    maxmana = character.maxmana;
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
    temp_stats = arr;
  }

let get_temp_value attr chr =
  match attr with
  | "maxhp" -> (
      match chr.temp_stats.(0) with
      | HP x, _ -> x
      | _ -> failwith "temp hp attr is not valid")
  | "maxmana" -> (
      match chr.temp_stats.(1) with
      | Mana x, _ -> x
      | _ -> failwith "temp mana attr is not valid")
  | "strength" -> (
      match chr.temp_stats.(2) with
      | Strength x, _ -> x
      | _ -> failwith "temp strength attr is not valid")
  | "defense" -> (
      match chr.temp_stats.(3) with
      | Defense x, _ -> x
      | _ -> failwith "temp defense attr is not valid")
  | "magic resist" -> (
      match chr.temp_stats.(4) with
      | MagicResist x, _ -> x
      | _ -> failwith "temp mag resist attr is not valid")
  | "speed" -> (
      match chr.temp_stats.(5) with
      | Speed x, _ -> x
      | _ -> failwith "temp speed attr is not valid")
  | "accuracy" -> (
      match chr.temp_stats.(6) with
      | Accuracy x, _ -> x
      | _ -> failwith "temp accuracy attr is not valid")
  | "magic power" -> (
      match chr.temp_stats.(7) with
      | MagicPower x, _ -> x
      | _ -> failwith "temp mag attr is not valid")
  | "luck" -> (
      match chr.temp_stats.(8) with
      | Luck x, _ -> x
      | _ -> failwith "temp mag attr is not valid")
  | _ -> failwith "not valid attr"

let get_name character = character.name

let start_character nme =
  {
    name = nme;
    hp = HP 100.;
    maxhp = HP 100.;
    mana = Mana 100.;
    maxmana = Mana 100.;
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
    skillset = [| Some icicle; Some piercing_light; None; None |];
    temp_stats =
      [|
        (HP 0., -1);
        (Mana 0., -1);
        (Strength 0., -1);
        (Defense 0., -1);
        (MagicResist 0., -1);
        (Speed 0., -1);
        (Accuracy 0., -1);
        (MagicPower 0., -1);
        (Luck 0., -1);
      |];
  }

let ( +* ) att amt =
  match att with
  | HP hp -> HP (hp +. amt)
  | Mana mp -> Mana (mp +. amt)
  | Strength str -> Strength (str +. amt)
  | Defense def -> Defense (def +. amt)
  | MagicResist mdef -> MagicResist (mdef +. amt)
  | Speed spd -> Speed (spd +. amt)
  | Accuracy acc -> Accuracy (acc +. amt)
  | MagicPower mag -> MagicPower (mag +. amt)
  | Luck luk -> Luck (luk +. amt)

let adjust amt ch = function
  | "hp" -> { ch with hp = ch.hp +* amt }
  | "mana" -> { ch with mana = ch.mana +* amt }
  | "strength" -> { ch with str = ch.str +* amt }
  | "defense" -> { ch with def = ch.def +* amt }
  | "magic resist" -> { ch with mr = ch.mr +* amt }
  | "speed" -> { ch with spd = ch.spd +* amt }
  | "accuracy" -> { ch with acc = ch.acc +* amt }
  | "magic power" -> { ch with mag = ch.mag +* amt }
  | "luck" -> { ch with luk = ch.luk +* amt }
  | _ -> raise UnknownAttribute

let adjust_set amt ch = function
  | "hp" -> { ch with hp = HP amt }
  | "mana" -> { ch with mana = Mana amt }
  | "strength" -> { ch with str = Strength amt }
  | "defense" -> { ch with def = Defense amt }
  | "magic resist" -> { ch with mr = MagicResist amt }
  | "speed" -> { ch with spd = Speed amt }
  | "accuracy" -> { ch with acc = Accuracy amt }
  | "magic power" -> { ch with mag = MagicPower amt }
  | "luck" -> { ch with luk = Luck amt }
  | _ -> raise UnknownAttribute

let level_up ch = { ch with exp = 0.; lvl = ch.lvl + 1 }
let cost_calc sk user = adjust sk.hp_cost (adjust sk.mp_cost user "mp") "hp"
let get_curr_attr attr chr = get_attribute attr chr +. get_temp_value attr chr
let get_from_tuple (a, b) = b

let get_attribute_val (at, _) =
  match at with
  | HP hp -> hp
  | Mana mp -> mp
  | Strength str -> str
  | Defense def -> def
  | MagicResist mdef -> mdef
  | Speed spd -> spd
  | Accuracy acc -> acc
  | MagicPower mag -> mag
  | Luck luk -> luk

let string_arr =
  [|
    "max HP";
    "max mana";
    "strength";
    "defense";
    "magic resist";
    "speed";
    "accuracy";
    "magic power";
    "luck";
  |]

let change_temps sk target =
  let arr = sk.attribute_affected in
  for i = 0 to Array.length arr - 1 do
    if get_from_tuple arr.(i) > 0 then (
      let amt = get_attribute_val arr.(i) in
      let word = if amt > 0. then "increased" else "decreased" in
      print_endline
        (target.name ^ "'s " ^ string_arr.(i) ^ " " ^ word ^ " by "
       ^ string_of_float amt);
      target.temp_stats.(i) <- arr.(i))
  done;
  if
    get_attribute_val (target.hp, -1)
    > get_attribute_val (target.maxhp, -1) +. get_attribute_val arr.(0)
  then (
    let att =
      get_attribute_val (target.maxhp, -1) +. get_attribute_val arr.(0)
    in
    print_endline ("Current HP truncated to " ^ string_of_float att);
    adjust_set att target "hp")
  else if
    get_attribute_val (target.mana, -1)
    > get_attribute_val (target.maxmana, -1) +. get_attribute_val arr.(1)
  then (
    let att =
      get_attribute_val (target.maxmana, -1) +. get_attribute_val arr.(1)
    in
    print_endline
      ("Current Mana truncated to "
      ^ string_of_float (get_attribute_val arr.(1)));
    adjust_set att target "mana")
  else target

let wait un =
  let i = ref 0 in
  while !i < 100 do
    match read_line () with
    | _ -> i := 100
  done

let use_skill sk user target =
  match sk.skill_type with
  | Magic ->
      if get_attribute "mana" user >= sk.mp_cost then
        let _ =
          if List.length user.enem_hit_chances = 0 then
            print_endline ("You used " ^ sk.name ^ " !")
        in
        let avoid = get_attribute "speed" target in
        let player_hit_chance =
          ((get_attribute "accuracy" user +. 60.) /. 100.0)
          -. (0.001 *. (avoid *. avoid))
        in
        let rand0 = Random.float 1. in
        let _ = wait 0 in
        if rand0 > player_hit_chance then
          if List.length user.enem_hit_chances = 0 then
            let _ = print_endline "You missed!" in
            (user, target, true)
          else
            let _ = print_endline (user.name ^ " missed!") in
            (target, user, true)
        else
          let dmg =
            (sk.base_dmg +. (unwrap user.mag *. sk.dmg_scaling))
            /. (1. +. (get_curr_attr "magic resist" user /. 50.))
          in
          let _ =
            print_endline
              (user.name ^ " dealt " ^ string_of_float dmg ^ " damage!")
          in
          let new_targ_stp1 = adjust (-.dmg) target "hp" in
          let new_usr = adjust (-.sk.mp_cost) user "mana" in
          let rand = Random.float 1. in
          let new_targ =
            if rand <= sk.chance_to_affect then change_temps sk new_targ_stp1
            else new_targ_stp1
          in
          let _ = wait () in
          if List.length user.enem_hit_chances != 0 then
            (new_targ, new_usr, true)
          else (new_usr, new_targ, true)
      else
        let _ =
          let nm =
            if List.length user.enem_hit_chances = 0 then "You don't"
            else user.name ^ " doesn't"
          in
          print_endline (nm ^ " have enough mana to use " ^ sk.name)
        in
        if List.length user.enem_hit_chances = 0 then (user, target, false)
        else (target, user, false)
  | Physical ->
      if get_attribute "hp" user > sk.hp_cost then
        let _ = print_endline ("You used " ^ sk.name ^ " !") in
        let dmg =
          (unwrap user.str +. (unwrap user.mag *. sk.dmg_scaling))
          /. (1. +. (unwrap target.mag /. 50.))
        in
        let new_targ = adjust (-.dmg) target "hp" in
        let new_usr = adjust (-.sk.mp_cost) target "mp" in
        (new_usr, new_targ, true)
      else
        let _ = print_endline ("You don't have enough hp to use " ^ sk.name) in
        if List.length user.enem_hit_chances = 0 then (user, target, false)
        else (target, user, false)
  | Status ->
      let _ = print_endline ("You used " ^ sk.name ^ " !") in
      (user, change_temps sk target, true)

[@@@warning "-8"]

let adjust_temps (attr, t) character =
  match attr with
  | HP hp -> (
      match character.temp_stats.(0) with
      | HP x, c ->
          let () = character.temp_stats.(0) <- (HP (x +. hp), c + t) in
          if hp < 0. then adjust (-.hp) character "hp"
          else adjust hp character "hp")
  | Mana mp -> (
      match character.temp_stats.(1) with
      | Mana x, c ->
          let () = character.temp_stats.(1) <- (Mana (x +. mp), c + t) in
          if mp < 0. then adjust (-.mp) character "mana"
          else adjust mp character "mp")
  | Strength str -> (
      match character.temp_stats.(2) with
      | Strength x, c ->
          let () = character.temp_stats.(2) <- (Strength (x +. str), c + t) in
          character)
  | Defense def -> (
      match character.temp_stats.(3) with
      | Defense x, c ->
          let () = character.temp_stats.(3) <- (Defense (x +. def), c + t) in
          character)
  | MagicResist mr -> (
      match character.temp_stats.(4) with
      | MagicResist x, c ->
          let () = character.temp_stats.(4) <- (MagicResist (x +. mr), c + t) in
          character)
  | Speed spd -> (
      match character.temp_stats.(5) with
      | Speed x, c ->
          let () = character.temp_stats.(5) <- (Speed (x +. spd), c + t) in
          character)
  | Accuracy acc -> (
      match character.temp_stats.(6) with
      | Accuracy x, c ->
          let () = character.temp_stats.(6) <- (Accuracy (x +. acc), c + t) in
          character)
  | MagicPower mag -> (
      match character.temp_stats.(7) with
      | MagicPower x, c ->
          let () = character.temp_stats.(7) <- (MagicPower (x +. mag), c + t) in
          character)
  | Luck luk -> (
      match character.temp_stats.(8) with
      | Luck x, c ->
          let () = character.temp_stats.(8) <- (Luck (x +. luk), c + t) in
          character)

let dots = "......................................."

let print_skills actor =
  let arr = actor.skillset in
  for i = 0 to Array.length arr - 1 do
    match arr.(i) with
    | Some sk ->
        let s = String.capitalize_ascii sk.name in
        let minus =
          if sk.mp_cost > 99. then String.length s + 1 else String.length s
        in
        print_endline
          ("● " ^ s
          ^ String.sub dots 0 (37 - minus)
          ^
          if sk.skill_type = Physical then
            string_of_int (int_of_float sk.hp_cost) ^ "HP..."
          else string_of_int (int_of_float sk.mp_cost) ^ " Mana")
    | None -> print_endline ("● " ^ dots ^ ".....")
  done
