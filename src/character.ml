exception No_skill

open Yojson.Basic.Util

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

type weapon = {
  name : string;
  statbuffs : (attribute * int) array;
  description : string;
}

type armor = weapon

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

type consumable = skill

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

exception UnknownAttribute
exception WrongAttribute

(*skill repo start*)

let return_type raw_type =
  match raw_type with
  | "Magic" -> Magic
  | "Physical" -> Physical
  | "Status" -> Status
  | _ -> Magic

let wait un =
  let i = ref 0 in
  while !i < 100 do
    match read_line () with
    | _ -> i := 100
  done

let return_attr json attr_name =
  let effect = json |> member (attr_name ^ "-effect") |> to_float in
  let turns = json |> member (attr_name ^ "-turns") |> to_int in
  match attr_name with
  | "hp" -> (HP effect, turns)
  | "mana" -> (Mana effect, turns)
  | "defense" -> (Defense effect, turns)
  | "strength" -> (Strength effect, turns)
  | "magicresist" -> (MagicResist effect, turns)
  | "speed" -> (Speed effect, turns)
  | "accuracy" -> (Accuracy effect, turns)
  | "magicpower" -> (MagicPower effect, turns)
  | "luck" -> (Luck effect, turns)
  | _ -> (HP effect, turns)

let parse_skill name json =
  {
    name;
    skill_type = return_type (json |> member "skill_type" |> to_string);
    attribute_affected =
      [|
        return_attr json "hp";
        return_attr json "mana";
        return_attr json "strength";
        return_attr json "defense";
        return_attr json "magicresist";
        return_attr json "speed";
        return_attr json "accuracy";
        return_attr json "magicpower";
        return_attr json "luck";
      |];
    description = json |> member "description" |> to_string;
    chance_to_affect = json |> member "change_to_affect" |> to_float;
    base_dmg = json |> member "base_dmg" |> to_float;
    dmg_scaling = json |> member "dmg_scailing" |> to_float;
    mp_cost = json |> member "mp_cost" |> to_float;
    hp_cost = json |> member "hp_cost" |> to_float;
  }

let get_skill_from_json skill_name =
  let data_prefix = "data" ^ Filename.dir_sep in

  let skills_raw = Yojson.Basic.from_file (data_prefix ^ "skills.json") in
  skills_raw |> member skill_name |> parse_skill skill_name

let icicle = get_skill_from_json "icicle"
let acid_spray = get_skill_from_json "acid spray"
let blood = get_skill_from_json "blood magic"
let minimize = get_skill_from_json "minimize"
let spin_slash = get_skill_from_json "spin slash"
let double_slash = get_skill_from_json "spin slash"
let headbutt = get_skill_from_json "headbutt"
let tsu = get_skill_from_json "tsunami"
let chainlight = get_skill_from_json "chain lightning"
let piercing_light = get_skill_from_json "piercing light"
let dark = get_skill_from_json "Nosferatu"

(*consumables repo*)

let make_consumable name =
  let itm = get_skill_from_json name in
  { name; item = itm; amt = -1 }

let lvl_1_spells = [| Some acid_spray; Some spin_slash |]
let lvl_2_spells = [| Some minimize; Some piercing_light |]
let lvl_3_spells = [| Some blood; Some headbutt |]
let lvl_4_spells = [| Some tsu; Some double_slash |]
let lvl_5_spells = [| Some chainlight; Some dark |]

(*skill repo end*)

(* consumables *)
(*consumables repo*)
let health_potion_bk = make_consumable "health potion"
let mana_potion_bk = make_consumable "mana potion"
let wrath_potion_bk = make_consumable "wrath_potion"
let magic_potion_bk = make_consumable "magic_potion"
let deal_with_devil_bk = make_consumable "deal_with_devil"
let get_enem_move_chance enem = enem.enem_hit_chances
let get_skills actor = actor.skillset
let get_inv actor = actor.inv

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

let get_attribute_val attr ch =
  match attr with
  | "maxhp" -> unwrap_attr ch.maxhp
  | "maxmana" -> unwrap_attr ch.maxmana
  | "hp" -> unwrap_attr ch.hp
  | "mana" -> unwrap_attr ch.mana
  | "strength" -> unwrap_attr ch.str
  | "defense" -> unwrap_attr ch.def
  | "magic resist" -> unwrap_attr ch.mr
  | "magic power" -> unwrap_attr ch.mag
  | "speed" -> unwrap_attr ch.spd
  | "accuracy" -> unwrap_attr ch.acc
  | "luck" -> unwrap_attr ch.luk
  | _ -> failwith "not valid attr"

let get_attribute_name attr =
  match attr with
  | HP _ -> "HP"
  | Mana _ -> "Mana"
  | Strength _ -> "Strength"
  | Defense _ -> "Defense"
  | MagicResist _ -> "Magic Resist"
  | Speed _ -> "Speed"
  | Accuracy _ -> "Accuracy"
  | MagicPower _ -> "Magic Power"
  | Luck _ -> "Luck"

let clear_temps ch =
  let arr = ch.temp_stats in
  for i = 0 to Array.length arr - 1 do
    let at, n = arr.(i) in
    if n = 1 then (
      arr.(i) <- (change_temp_attr_overwrite 0. at, -1);
      let nm =
        if List.length ch.enem_hit_chances = 0 then "Player " else ch.name ^ " "
      in
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        (nm ^ get_attribute_name at ^ " reverted.\n");
      wait ())
    else if n > 1 then arr.(i) <- (at, n - 1)
  done;
  { ch with temp_stats = arr }

let get_temp_value attr_str chr =
  match attr_str with
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
      | _ -> failwith "temp luk attr is not valid")
  | _ -> failwith "not valid attr"

let get_name ch = ch.name

let start_character nme =
  {
    name = nme;
    hp = HP 100.;
    maxhp = HP 100.;
    mana = Mana 100.;
    maxmana = Mana 100.;
    exp = 0;
    lvl = 1;
    str = Strength 10.;
    def = Defense 10.;
    mr = MagicResist 10.;
    spd = Speed 10.;
    acc = Accuracy 10.;
    mag = MagicPower 10.;
    luk = Luck 10.;
    enem_hit_chances = [];
    skillset = [| Some icicle; None; None; None |];
    inv =
      [|
        { health_potion_bk with amt = 3 };
        { mana_potion_bk with amt = 1 };
        { wrath_potion_bk with amt = 0 };
        { magic_potion_bk with amt = 0 };
        { deal_with_devil_bk with amt = 0 };
        { health_potion_bk with amt = 0 };
      |];
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

let stat_randomizer attr_str ch =
  let og = get_attribute_val attr_str ch in
  let x = Float.round (og +. (10. *. Float.pow (Random.float 1.) 2.)) in
  let _ =
    if x > og then
      ANSITerminal.print_string [ ANSITerminal.green ]
        (String.capitalize_ascii attr_str
        ^ " increased to "
        ^ string_of_int (int_of_float x)
        ^ "\n")
    else
      ANSITerminal.print_string [ ANSITerminal.red ]
        (String.capitalize_ascii "Bad luck. Your "
        ^ attr_str ^ " didn't increase.\n")
  in
  x

let print_attrs () =
  let _ = print_string "● strength" in
  let _ = wait () in
  let _ = print_string "● defense" in
  let _ = wait () in
  let _ = print_string "● magic resist" in
  let _ = wait () in
  let _ = print_string "● speed" in
  let _ = wait () in
  let _ = print_string "● accuracy" in
  let _ = wait () in
  let _ = print_string "● magic power" in
  let _ = wait () in
  print_string "● luck\n"

let level_attr actor attr_str =
  match attr_str with
  | "strength" ->
      { actor with str = Strength (stat_randomizer "strength" actor) }
  | "defense" -> { actor with def = Defense (stat_randomizer "defense" actor) }
  | "magic resist" ->
      { actor with mr = MagicResist (stat_randomizer "magic resist" actor) }
  | "speed" -> { actor with spd = Speed (stat_randomizer "speed" actor) }
  | "accuracy" ->
      { actor with acc = Accuracy (stat_randomizer "accuracy" actor) }
  | "magic power" ->
      { actor with mag = MagicPower (stat_randomizer "magic power" actor) }
  | "luck" -> { actor with luk = Luck (stat_randomizer "luck" actor) }
  | _ -> raise UnknownAttribute

let rec upgrade_menu actor counter ran =
  let _ =
    if not ran then
      let _ =
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "You can level up 3 attributes:\n"
      in
      print_attrs ()
  in
  let a1, counter =
    if counter = 0 then
      let _ =
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "\nChoose your first attribute to improve.\n"
      in
      let s = read_line () in
      try (level_attr actor s, counter + 1)
      with UnknownAttribute ->
        let _ =
          ANSITerminal.print_string [ ANSITerminal.red ]
            "That's not a valid attribute. Please try again.\n"
        in
        upgrade_menu actor 0 true
    else (actor, counter)
  in
  let a2, counter =
    if counter = 1 then
      let _ =
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "\nChoose your second attribute to improve.\n"
      in
      let s = read_line () in
      try (level_attr a1 s, counter + 1)
      with UnknownAttribute ->
        let _ =
          ANSITerminal.print_string [ ANSITerminal.red ]
            "That's not a valid attribute. Please try again.\n"
        in
        upgrade_menu a1 1 true
    else (a1, counter)
  in
  let a3, counter =
    if counter = 2 then
      let _ =
        ANSITerminal.print_string [ ANSITerminal.blue ]
          "\nChoose your third attribute to improve.\n"
      in
      let s = read_line () in
      try (level_attr a2 s, counter + 1)
      with UnknownAttribute ->
        let _ =
          ANSITerminal.print_string [ ANSITerminal.red ]
            "That's not a valid attribute. Please try again.\n"
        in
        upgrade_menu a2 2 true
    else (a2, counter)
  in
  (a3, -1)

let dots = "......................................."

let print_skills (arr : skill option array) =
  for i = 0 to Array.length arr - 1 do
    match arr.(i) with
    | None -> print_endline ("● " ^ dots ^ ".....")
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
  done

let rec forget_skill (arr : skill option array) sk =
  match read_line () with
  | s when s = (unwrap_skill arr.(0)).name ->
      arr.(0) <- Some sk;
      print_endline ("Learned " ^ sk.name ^ "!")
  | s when s = (unwrap_skill arr.(1)).name ->
      arr.(1) <- Some sk;
      print_endline ("Learned " ^ sk.name ^ "!")
  | s when s = (unwrap_skill arr.(2)).name ->
      arr.(2) <- Some sk;
      print_endline ("Learned " ^ sk.name ^ "!")
  | s when s = (unwrap_skill arr.(3)).name ->
      arr.(3) <- Some sk;
      print_endline ("Learned " ^ sk.name ^ "!")
  | s when s = "none" -> print_endline "Didn't learn new skill."
  | _ ->
      print_endline "That is not a valid skill. Please try again.";
      forget_skill arr sk

let learn_skill_helper (arr : skill option array) sk =
  let i = ref 0 in
  while !i < 4 do
    if arr.(!i) = None then (
      arr.(!i) <- Some sk;
      i := 100;
      print_endline ("Learned " ^ sk.name ^ "!"))
    else if !i = 3 then (
      print_endline
        "You cannot learn any more skills. Select a skill to discard, or type \
         [none] to keep current skills";
      print_skills arr;
      forget_skill arr sk);
    i := !i + 1
  done

let rec learn_skill (curr_skills : skill option array) ran =
  if not ran then
    ANSITerminal.print_string [ ANSITerminal.magenta ]
      "\nChoose a spell to learn:\n";
  let arr = lvl_2_spells in
  let _ = print_skills arr in
  match read_line () with
  | s when s = (unwrap_skill arr.(0)).name ->
      learn_skill_helper curr_skills (unwrap_skill arr.(0))
  | s when s = (unwrap_skill arr.(1)).name ->
      learn_skill_helper curr_skills (unwrap_skill arr.(1))
  | s when s = "info " ^ (unwrap_skill arr.(0)).name ->
      ANSITerminal.print_string [ ANSITerminal.default ]
        ((unwrap_skill arr.(0)).description ^ "\n");
      learn_skill curr_skills true
  | s when s = "info " ^ (unwrap_skill arr.(1)).name ->
      ANSITerminal.print_string [ ANSITerminal.default ]
        ("\n" ^ (unwrap_skill arr.(1)).description ^ "\n\n");
      learn_skill curr_skills true
  | _ ->
      print_endline "That is not a valid skill. Please try again.";
      learn_skill curr_skills ran

let level_up actor =
  Random.self_init ();
  ANSITerminal.print_string [ ANSITerminal.green ] "You have leveled up!\n";
  let _ = wait () in
  let hpmax =
    let x =
      Float.round (get_attribute_val "maxhp" actor +. (20. *. Random.float 1.))
    in
    let _ =
      if x > 0. then
        ANSITerminal.print_string []
          ("● Max HP increased to " ^ string_of_int (int_of_float x) ^ "\n")
    in
    HP x
  in
  let manamax =
    let x =
      Float.round (get_attribute_val "maxmana" actor +. (30. *. Random.float 1.))
    in
    let _ =
      if x > 0. then
        ANSITerminal.print_string []
          ("● Max Mana increased to " ^ string_of_int (int_of_float x) ^ "\n\n")
    in
    Mana x
  in
  let actor_final, _ = upgrade_menu actor 0 false in
  let skills =
    if (actor.lvl + 1) mod 2 = 0 then learn_skill actor.skillset false;
    actor.skillset
  in
  {
    actor_final with
    hp = hpmax;
    maxhp = hpmax;
    mana = manamax;
    maxmana = manamax;
    skillset = skills;
  }

let cost_calc sk user = adjust sk.hp_cost (adjust sk.mp_cost user "mp") "hp"

let get_total_attr_val attr chr =
  let a = get_attribute_val attr chr in
  let b = get_temp_value attr chr in
  a +. b

let get_from_tuple (a, b) = b

let get_temp_attribute_val (at, _) =
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

let change_temps_from_skill sk target =
  let arr = sk.attribute_affected in
  for i = 0 to Array.length arr - 1 do
    if get_from_tuple arr.(i) > 0 then (
      let amt = get_temp_attribute_val arr.(i) in
      let word = if amt > 0. then "increased" else "decreased" in
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        (target.name ^ "'s " ^ string_arr.(i) ^ " " ^ word ^ " by "
       ^ string_of_float amt ^ "\n");
      target.temp_stats.(i) <- arr.(i))
  done;
  if
    get_temp_attribute_val (target.hp, -1)
    > get_temp_attribute_val (target.maxhp, -1)
      +. get_temp_attribute_val arr.(0)
  then (
    let att =
      get_temp_attribute_val (target.maxhp, -1)
      +. get_temp_attribute_val arr.(0)
    in
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("Current HP truncated to " ^ string_of_float att ^ "\n");
    adjust_set att target "hp")
  else if
    get_temp_attribute_val (target.mana, -1)
    > get_temp_attribute_val (target.maxmana, -1)
      +. get_temp_attribute_val arr.(1)
  then (
    let att =
      get_temp_attribute_val (target.maxmana, -1)
      +. get_temp_attribute_val arr.(1)
    in
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("Current Mana truncated to "
      ^ string_of_float (get_temp_attribute_val arr.(1))
      ^ "\n");
    adjust_set att target "mana")
  else target

let wait un =
  let i = ref 0 in
  while !i < 100 do
    match read_line () with
    | _ -> i := 100
  done

let use_skill sk user target =
  Random.self_init ();
  match sk.skill_type with
  | Magic ->
      if get_attribute_val "mana" user >= sk.mp_cost then (
        let _ =
          if List.length user.enem_hit_chances = 0 then
            ANSITerminal.print_string [ ANSITerminal.blue ]
              ("You used " ^ sk.name ^ " !")
        in
        let new_usr = adjust (-.sk.mp_cost) user "mana" in

        let avoid = get_attribute_val "speed" target in
        let player_hit_chance =
          if sk.mp_cost < 0. then 1.
          else
            ((get_total_attr_val "accuracy" user +. 60.) /. 100.0)
            -. (0.001 *. (avoid *. avoid))
        in
        let rand0 = Random.float 1. in
        if sk.mp_cost >= 0. then wait 0;
        if rand0 > player_hit_chance then
          if List.length user.enem_hit_chances = 0 then
            let _ =
              ANSITerminal.print_string [ ANSITerminal.default ]
                "You missed!\n\n"
            in
            (new_usr, target, true)
          else
            let _ =
              ANSITerminal.print_string [ ANSITerminal.white ]
                (user.name ^ " missed!\n\n")
            in
            (target, new_usr, true)
        else
          let dmg =
            let raw =
              (sk.base_dmg +. (3. *. unwrap_attr user.mag *. sk.dmg_scaling))
              /. (1.
                 +.
                 if get_total_attr_val "magic resist" target /. 10. < 0. then 0.
                 else get_total_attr_val "magic resist" target /. 10.)
            in
            if raw > 0. then raw else 0.
          in
          let _ =
            if sk.mp_cost >= 0. then
              if List.length user.enem_hit_chances = 0 then
                ANSITerminal.print_string [ ANSITerminal.white ]
                  (user.name ^ " dealt " ^ string_of_float dmg ^ " damage!\n")
              else
                ANSITerminal.print_string [ ANSITerminal.white ]
                  (user.name ^ " dealt " ^ string_of_float dmg ^ " damage!\n")
          in
          let new_targ_stp1 = adjust (-.dmg) target "hp" in
          let rand = Random.float 1. in
          let new_targ =
            if rand <= sk.chance_to_affect then
              change_temps_from_skill sk new_targ_stp1
            else new_targ_stp1
          in
          let _ = if sk.mp_cost >= 0. then wait () in
          if List.length user.enem_hit_chances != 0 then
            (new_targ, new_usr, true)
          else (new_usr, new_targ, true))
      else
        let _ =
          let nm, lst =
            if List.length user.enem_hit_chances = 0 then
              ("You don't", [ ANSITerminal.yellow ])
            else (user.name ^ " doesn't", [ ANSITerminal.red ])
          in
          ANSITerminal.print_string lst
            ("\n" ^ nm ^ " have enough mana to use " ^ sk.name ^ "\n")
        in
        if List.length user.enem_hit_chances = 0 then (user, target, false)
        else (target, user, false)
  | Physical ->
      if get_attribute_val "hp" user > sk.hp_cost then (
        let avoid = get_attribute_val "speed" target in
        let player_hit_chance =
          if sk.mp_cost < 0. then 1.
          else
            ((get_total_attr_val "accuracy" user +. 60.) /. 100.0)
            -. (0.001 *. (avoid *. avoid))
        in
        let rand0 = Random.float 1. in
        let _ = if sk.hp_cost >= 0. then wait 0 in
        if rand0 <= player_hit_chance then (
          let _ =
            ANSITerminal.print_string [ ANSITerminal.blue ]
              ("You used " ^ sk.name ^ "!\n")
          in
          let dmg =
            let raw =
              (unwrap_attr user.str +. (unwrap_attr user.mag *. sk.dmg_scaling))
              /. (1. +. (unwrap_attr target.mag /. 50.))
            in
            if raw > 0. then raw else 0.
          in
          let new_targ = adjust (-.dmg) target "hp" in
          let new_usr = adjust (-.sk.hp_cost) target "hp" in
          print_endline "";
          (new_usr, new_targ, true))
        else
          let new_usr = adjust (-.sk.hp_cost) target "hp" in
          print_endline (user.name ^ " missed!");
          if List.length new_usr.enem_hit_chances = 0 then
            (new_usr, target, true)
          else (target, new_usr, true))
      else
        let _ =
          ANSITerminal.print_string [ ANSITerminal.yellow ]
            ("You don't have enough hp to use " ^ sk.name ^ "\n")
        in
        if List.length user.enem_hit_chances = 0 then (user, target, false)
        else (target, user, false)
  | Status ->
      if sk.mp_cost <= unwrap_attr user.mana then
        let _ =
          if List.length user.enem_hit_chances = 0 then
            ANSITerminal.print_string [ ANSITerminal.blue ]
              ("You used " ^ sk.name ^ "!")
          else
            ANSITerminal.print_string [ ANSITerminal.red ]
              (user.name ^ " used " ^ sk.name ^ " !\n")
        in
        let new_usr = adjust (-.sk.mp_cost) user "mana" in
        let avoid = get_attribute_val "speed" target in
        let player_hit_chance =
          if sk.mp_cost < 0. then 1.
          else
            ((get_total_attr_val "accuracy" user +. 60.) /. 100.0)
            -. (0.001 *. (avoid *. avoid))
        in
        let rand0 = Random.float 1. in
        let _ = wait 0 in
        if rand0 <= player_hit_chance then
          if List.length user.enem_hit_chances = 0 then
            (new_usr, change_temps_from_skill sk target, true)
          else (change_temps_from_skill sk target, new_usr, true)
        else
          let _ =
            let name =
              if List.length user.enem_hit_chances = 0 then "You" else user.name
            in
            print_endline (name ^ " missed!\n")
          in
          if List.length new_usr.enem_hit_chances = 0 then
            (new_usr, target, true)
          else (target, new_usr, true)
      else
        let _ =
          let nm, lst =
            if List.length user.enem_hit_chances = 0 then
              ("You don't", [ ANSITerminal.yellow ])
            else (user.name ^ " doesn't", [ ANSITerminal.red ])
          in
          ANSITerminal.print_string lst
            (nm ^ " have enough mana to use " ^ sk.name ^ "\n")
        in
        if List.length user.enem_hit_chances = 0 then (user, target, false)
        else (target, user, false)

let use_consumable csbl ch idx =
  let new_ch, _, _ = use_skill csbl ch ch in
  let new_ch' =
    if get_attribute_val "hp" new_ch > get_attribute_val "maxhp" new_ch then
      { new_ch with hp = new_ch.maxhp }
    else if get_attribute_val "mana" new_ch > get_attribute_val "maxmana" new_ch
    then { new_ch with mana = new_ch.maxmana }
    else new_ch
  in
  let x = new_ch'.inv.(idx) in
  new_ch'.inv.(idx) <- { x with amt = x.amt - 1 };
  new_ch'

let get_description_skill s = s.description
let get_description_item i = get_description_skill i

[@@@warning "-8"]

let adjust_temps (attr, t) ch =
  match attr with
  | HP hp -> (
      match ch.temp_stats.(0) with
      | HP x, c ->
          let ct = if c >= 0 then c else 0 in
          let () = ch.temp_stats.(0) <- (HP (x +. hp), t + ct) in
          if hp < 0. then adjust (-.hp) ch "hp" else ch
      | _ -> raise UnknownAttribute)
  | Mana mp -> (
      match ch.temp_stats.(1) with
      | Mana x, c ->
          let ct = if c >= 0 then c else 0 in
          let () = ch.temp_stats.(1) <- (Mana (mp +. x), t + ct) in
          if mp < 0. then adjust (-.mp) ch "mana" else ch
      | _ -> raise UnknownAttribute)
  | Strength str -> (
      match ch.temp_stats.(2) with
      | Strength x, c ->
          let ct = if c >= 0 then c else 0 in
          let () = ch.temp_stats.(2) <- (Strength (str +. x), t + ct) in
          ch
      | _ -> raise UnknownAttribute)
  | Defense def -> (
      match ch.temp_stats.(3) with
      | Defense x, c ->
          let ct = if c >= 0 then c else 0 in
          let () = ch.temp_stats.(3) <- (Defense (def +. x), t + ct) in
          ch
      | _ -> raise UnknownAttribute)
  | MagicResist mr -> (
      match ch.temp_stats.(4) with
      | MagicResist x, c ->
          let ct = if c >= 0 then c else 0 in
          let () = ch.temp_stats.(4) <- (MagicResist (mr +. x), t + ct) in
          ch
      | _ -> raise UnknownAttribute)
  | Speed spd -> (
      match ch.temp_stats.(5) with
      | Speed x, c ->
          let ct = if c >= 0 then c else 0 in
          let () = ch.temp_stats.(5) <- (Speed (spd +. x), t + ct) in
          ch
      | _ -> raise UnknownAttribute)
  | Accuracy acc -> (
      match ch.temp_stats.(6) with
      | Accuracy x, c ->
          let ct = if c >= 0 then c else 0 in
          let () = ch.temp_stats.(6) <- (Accuracy (acc +. x), t + ct) in
          ch
      | _ -> raise UnknownAttribute)
  | MagicPower mag -> (
      match ch.temp_stats.(7) with
      | MagicPower x, c ->
          let ct = if c >= 0 then c else 0 in
          let () = ch.temp_stats.(7) <- (MagicPower (mag +. x), t + ct) in
          ch
      | _ -> raise UnknownAttribute)
  | Luck luk -> (
      match ch.temp_stats.(8) with
      | Luck x, c ->
          let ct = if c >= 0 then c else 0 in
          let () = ch.temp_stats.(8) <- (Luck (luk +. x), t + ct) in
          ch
      | _ -> raise UnknownAttribute)
