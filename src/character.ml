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
let double_slash = get_skill_from_json "double slash"
let headbutt = get_skill_from_json "headbutt"
let tsu = get_skill_from_json "tsunami"
let chainlight = get_skill_from_json "chain lightning"
let piercing_light = get_skill_from_json "piercing light"
let nosferatu = get_skill_from_json "nosferatu"

(*consumables repo*)

let make_consumable name =
  let itm = get_skill_from_json name in
  { name; item = itm; amt = -1 }

let lvl_1_spells = [| Some acid_spray; Some spin_slash |]
let lvl_2_spells = [| Some minimize; Some piercing_light |]
let lvl_3_spells = [| Some blood; Some headbutt |]
let lvl_4_spells = [| Some tsu; Some double_slash |]
let lvl_5_spells = [| Some chainlight; Some nosferatu |]

(*skill repo end*)

(* consumables *)
(*consumables repo*)
let health_potion_bk = make_consumable "health potion"
let mana_potion_bk = make_consumable "mana potion"
let wrath_potion_bk = make_consumable "potion of wrath"
let magic_potion_bk = make_consumable "spellflux elixir"
let deal_with_devil_bk = make_consumable "deal with devil"
let superspeed_bk = make_consumable "superspeed serum"
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

let match_skill skill =
  match skill with
  | "None" -> None
  | "icicle" -> Some icicle
  | "acid spray" -> Some acid_spray
  | "blood magic" -> Some blood
  | "minimize" -> Some minimize
  | "spin slash" -> Some spin_slash
  | "double slash" -> Some double_slash
  | "headbutt" -> Some headbutt
  | "tsunami" -> Some tsu
  | "chainlight" -> Some chainlight
  | "piercing light" -> Some piercing_light
  | "nosferatu" -> Some nosferatu
  | _ -> None

let parse_character nme hit_chances =
  let data_prefix = "data" ^ Filename.dir_sep in

  let skills_raw = Yojson.Basic.from_file (data_prefix ^ "characters.json") in
  let json_char = skills_raw |> member nme in

  let hp_val = json_char |> member "hp" |> to_float in
  let maxhp_val = json_char |> member "maxhp" |> to_float in
  let mana_val = json_char |> member "mana" |> to_float in
  let maxmana_val = json_char |> member "maxmana" |> to_float in
  let exp_val = json_char |> member "exp" |> to_int in
  let lvl_val = json_char |> member "lvl" |> to_int in
  let str_val = json_char |> member "str" |> to_float in
  let mr_val = json_char |> member "mr" |> to_float in

  let def_val = json_char |> member "def" |> to_float in
  let spd_val = json_char |> member "spd" |> to_float in
  let acc_val = json_char |> member "acc" |> to_float in
  let mag_val = json_char |> member "mag" |> to_float in
  let luk_val = json_char |> member "luk" |> to_float in
  let enemy_hit_chances_val = hit_chances in
  let skill1_val =
    json_char |> member "skillset1" |> to_string |> match_skill
  in
  let skill2_val =
    json_char |> member "skillset2" |> to_string |> match_skill
  in
  let skill3_val =
    json_char |> member "skillset3" |> to_string |> match_skill
  in
  let skill4_val =
    json_char |> member "skillset4" |> to_string |> match_skill
  in
  let hpotion_val = json_char |> member "health_potion" |> to_int in
  let mana_potion_val = json_char |> member "mana_potion" |> to_int in
  let wrath_potion_val = json_char |> member "wrath_potion" |> to_int in
  let magic_potion_val = json_char |> member "magic_potion" |> to_int in
  let superspeed_val = json_char |> member "super_speed" |> to_int in
  let deal_with_devil_val = json_char |> member "deal_with_devil" |> to_int in
  {
    name = nme;
    hp = HP hp_val;
    maxhp = HP maxhp_val;
    mana = Mana mana_val;
    maxmana = Mana maxmana_val;
    exp = exp_val;
    lvl = lvl_val;
    str = Strength str_val;
    def = Defense def_val;
    mr = MagicResist mr_val;
    spd = Speed spd_val;
    acc = Accuracy acc_val;
    mag = MagicPower mag_val;
    luk = Luck luk_val;
    enem_hit_chances = enemy_hit_chances_val;
    skillset = [| skill1_val; skill2_val; skill3_val; skill4_val |];
    inv =
      [|
        { health_potion_bk with amt = hpotion_val };
        { mana_potion_bk with amt = mana_potion_val };
        { wrath_potion_bk with amt = wrath_potion_val };
        { magic_potion_bk with amt = magic_potion_val };
        { superspeed_bk with amt = superspeed_val };
        { deal_with_devil_bk with amt = deal_with_devil_val };
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

let start_character nme =
  let raw = parse_character "start_character" [] in
  { raw with name = nme }

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
            string_of_int (int_of_float sk.hp_cost) ^ " HP..."
          else string_of_int (int_of_float sk.mp_cost) ^ " Mana")
  done

let forget_skill_helper (arr : skill option array) sk i =
  arr.(i) <- Some sk;
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    ("Learned " ^ sk.name ^ "! Please return to the GUI.")

let rec forget_skill (arr : skill option array) sk =
  match read_line () with
  | s when s = (unwrap_skill arr.(0)).name -> forget_skill_helper arr sk 0
  | s when s = (unwrap_skill arr.(1)).name -> forget_skill_helper arr sk 1
  | s when s = (unwrap_skill arr.(2)).name -> forget_skill_helper arr sk 2
  | s when s = (unwrap_skill arr.(3)).name -> forget_skill_helper arr sk 3
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
      print_endline ("Learned " ^ sk.name ^ "! Please return to the GUI."))
    else if !i = 3 then (
      print_endline
        "You cannot learn any more skills. Select a skill to discard, or type \
         [none] to keep current skills";
      print_skills arr;
      forget_skill arr sk);
    i := !i + 1
  done

let rec learn_skill (curr_skills : skill option array) ran lvl =
  if not ran then
    ANSITerminal.print_string [ ANSITerminal.magenta ]
      "\nChoose a skill to learn:\n";
  let arr =
    if lvl = 2 then lvl_1_spells
    else if lvl = 3 then lvl_2_spells
    else if lvl = 4 then lvl_3_spells
    else if lvl = 5 then lvl_4_spells
    else lvl_5_spells
  in
  let _ = print_skills arr in
  match read_line () with
  | s when s = (unwrap_skill arr.(0)).name ->
      learn_skill_helper curr_skills (unwrap_skill arr.(0))
  | s when s = (unwrap_skill arr.(1)).name ->
      learn_skill_helper curr_skills (unwrap_skill arr.(1))
  | s when s = "info " ^ (unwrap_skill arr.(0)).name ->
      ANSITerminal.print_string [ ANSITerminal.default ]
        ((unwrap_skill arr.(0)).description ^ "\n");
      learn_skill curr_skills true lvl
  | s when s = "info " ^ (unwrap_skill arr.(1)).name ->
      ANSITerminal.print_string [ ANSITerminal.default ]
        ("\n" ^ (unwrap_skill arr.(1)).description ^ "\n\n");
      learn_skill curr_skills true lvl
  | _ ->
      print_endline "That is not a valid skill. Please try again.";
      learn_skill curr_skills ran lvl

let level_up actor =
  Random.self_init ();
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("You have leveled up! Now level " ^ string_of_int (actor.lvl + 1) ^ "\n");
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
    let _ =
      if actor.lvl + 1 <= 6 then learn_skill actor.skillset false (actor.lvl + 1)
    in
    actor.skillset
  in
  {
    actor_final with
    lvl = actor_final.lvl + 1;
    exp = 0;
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

let clear_temps ch =
  let arr = !ch.temp_stats in
  for i = 0 to Array.length arr - 1 do
    let at, n = arr.(i) in
    if n = 1 then (
      arr.(i) <- (change_temp_attr_overwrite 0. at, -1);
      let nm =
        if List.length !ch.enem_hit_chances = 0 then "Player "
        else !ch.name ^ " "
      in
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        (nm ^ get_attribute_name at ^ " reverted.\n");
      wait ();
      if i = 0 && get_attribute_val "hp" !ch > get_total_attr_val "maxhp" !ch
      then (
        print_endline
          ("HP truncated to "
          ^ string_of_float (get_total_attr_val "maxhp" !ch)
          ^ "\n");
        ch := { !ch with hp = HP (get_total_attr_val "maxhp" !ch) });
      if
        i = 1 && get_attribute_val "mana" !ch > get_total_attr_val "maxmana" !ch
      then (
        print_endline
          ("Mana truncated to "
          ^ string_of_float (get_total_attr_val "maxmana" !ch)
          ^ "\n");
        ch := { !ch with mana = Mana (get_total_attr_val "maxmana" !ch) }))
    else if n > 1 then arr.(i) <- (at, n - 1)
  done;
  { !ch with temp_stats = arr }

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

let calculate_new_temps x y c t =
  let amt, ct =
    if c != -1 then
      if y < 0. && x > 0. then (x +. y, c)
      else if y < 0. && x < 0. then (x +. y, c)
      else if y > 0. && x < 0. then (y, t)
      else (y +. x, c + t)
    else (y, t)
  in
  (amt, ct)

let adjust_temps (attr, t) ch =
  match attr with
  | HP y -> (
      match ch.temp_stats.(0) with
      | HP x, c ->
          let amt, ct = calculate_new_temps x y c t in
          let () = ch.temp_stats.(0) <- (HP amt, ct) in
          if y < 0. then adjust (-.y) ch "hp" else ch
      | _ -> raise UnknownAttribute)
  | Mana y -> (
      match ch.temp_stats.(1) with
      | Mana x, c ->
          let amt, ct =
            if c != -1 then
              if y < 0. && x > 0. then (x +. y, c)
              else if y < 0. && x < 0. then (x +. y, c)
              else if y > 0. && x < 0. then (y, t)
              else (y +. x, c + t)
            else (y, t)
          in
          let () = ch.temp_stats.(1) <- (Mana amt, ct) in
          if y < 0. then adjust (-.y) ch "mana" else ch
      | _ -> raise UnknownAttribute)
  | Strength y -> (
      match ch.temp_stats.(2) with
      | Strength x, c ->
          let amt, ct = calculate_new_temps x y c t in
          let () = ch.temp_stats.(2) <- (Strength amt, ct) in
          ch
      | _ -> raise UnknownAttribute)
  | Defense y -> (
      match ch.temp_stats.(3) with
      | Defense x, c ->
          let amt, ct = calculate_new_temps x y c t in
          let () = ch.temp_stats.(3) <- (Defense amt, ct) in
          ch
      | _ -> raise UnknownAttribute)
  | MagicResist y -> (
      match ch.temp_stats.(4) with
      | MagicResist x, c ->
          let amt, ct = calculate_new_temps x y c t in
          let () = ch.temp_stats.(4) <- (MagicResist amt, ct) in
          ch
      | _ -> raise UnknownAttribute)
  | Speed y -> (
      match ch.temp_stats.(5) with
      | Speed x, c ->
          let amt, ct = calculate_new_temps x y c t in
          let () = ch.temp_stats.(5) <- (Speed amt, ct) in
          ch
      | _ -> raise UnknownAttribute)
  | Accuracy y -> (
      match ch.temp_stats.(6) with
      | Accuracy x, c ->
          let amt, ct = calculate_new_temps x y c t in
          let () = ch.temp_stats.(6) <- (Accuracy amt, ct) in
          ch
      | _ -> raise UnknownAttribute)
  | MagicPower y -> (
      match ch.temp_stats.(7) with
      | MagicPower x, c ->
          let amt, ct = calculate_new_temps x y c t in
          let () = ch.temp_stats.(7) <- (MagicPower amt, ct) in
          ch
      | _ -> raise UnknownAttribute)
  | Luck y -> (
      match ch.temp_stats.(8) with
      | Luck x, c ->
          let amt, ct = calculate_new_temps x y c t in
          let () = ch.temp_stats.(8) <- (Luck amt, ct) in
          ch
      | _ -> raise UnknownAttribute)

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
      let _ = adjust_temps arr.(i) target in
      ())
  done;
  if get_attribute_val "hp" target > get_total_attr_val "maxhp" target then (
    let att = get_total_attr_val "maxhp" target in
    ANSITerminal.print_string [ ANSITerminal.default ]
      ("Current HP truncated to " ^ string_of_float att ^ "\n");
    adjust_set att target "hp")
  else if get_attribute_val "mana" target > get_total_attr_val "maxmana" target
  then (
    let att = get_total_attr_val "maxmana" target in
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("Current Mana truncated to "
      ^ string_of_float (get_total_attr_val "maxmana" target)
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
            let suffix = if sk.mp_cost < 0. then "\n" else "" in
            ANSITerminal.print_string [ ANSITerminal.blue ]
              ("You used " ^ sk.name ^ "!" ^ suffix)
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
          let _ = wait () in
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
            ("\n" ^ nm ^ " have enough mana to use " ^ sk.name ^ "\n\n")
        in
        if List.length user.enem_hit_chances = 0 then (user, target, false)
        else (target, user, false)
  | Physical ->
      if get_attribute_val "hp" user > sk.hp_cost then
        let avoid = get_attribute_val "speed" target in
        let player_hit_chance =
          if sk.hp_cost < 0. || sk.dmg_scaling = 0.0 then 1.
          else
            ((get_total_attr_val "accuracy" user +. 60.) /. 100.0)
            -. (0.001 *. (avoid *. avoid))
        in
        let _ =
          if List.length user.enem_hit_chances = 0 then
            ANSITerminal.print_string [ ANSITerminal.blue ]
              ("You used " ^ sk.name ^ "!")
        in
        let rand0 = Random.float 1. in
        let _ = if sk.hp_cost >= 0. then wait 0 in
        if rand0 <= player_hit_chance then (
          let dmg =
            if sk.dmg_scaling = 0. then 0.
            else
              let raw =
                (unwrap_attr user.str
                +. (unwrap_attr user.mag *. sk.dmg_scaling))
                /. (1. +. (unwrap_attr target.mag /. 50.))
              in
              if raw > 0. then raw else 0.
          in
          let new_targ_stp1 = adjust (-.dmg) target "hp" in
          let _ =
            if sk.dmg_scaling > 0. then
              print_string ("Dealt " ^ string_of_float dmg ^ " damage.\n")
            else if sk.hp_cost > 0. then (
              print_string
                ("\nSacrificed " ^ string_of_float sk.hp_cost ^ " HP.\n");
              wait ())
            else
              print_endline
                ("\nRestored "
                ^ string_of_int (int_of_float (-1. *. sk.hp_cost))
                ^ " HP")
          in
          let rand = Random.float 1. in
          let new_targ =
            if rand <= sk.chance_to_affect then
              let tg = change_temps_from_skill sk new_targ_stp1 in
              let _ = wait () in
              tg
            else new_targ_stp1
          in
          let new_usr = adjust (-.sk.hp_cost) user "hp" in
          print_endline "";
          if List.length new_usr.enem_hit_chances = 0 then
            (new_usr, new_targ, true)
          else (new_targ, new_usr, true))
        else
          let new_usr = adjust (-.sk.hp_cost) user "hp" in
          let nm, a, b =
            if List.length new_usr.enem_hit_chances = 0 then
              ("You", new_usr, target)
            else (user.name, target, new_usr)
          in
          let _ = print_endline (nm ^ " missed!\n") in
          (a, b, true)
      else
        let _ =
          let nm =
            if List.length user.enem_hit_chances = 0 then "\nYou don't "
            else "\n" ^ user.name ^ " doesn't "
          in
          ANSITerminal.print_string [ ANSITerminal.yellow ]
            (nm ^ "have enough hp to use " ^ sk.name ^ "\n\n")
        in
        if List.length user.enem_hit_chances = 0 then (user, target, false)
        else (target, user, false)
  | Status ->
      if sk.mp_cost <= unwrap_attr user.mana then
        let _ =
          if List.length user.enem_hit_chances = 0 then
            ANSITerminal.print_string [ ANSITerminal.blue ]
              ("You used " ^ sk.name ^ "!")
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
              ("\nYou don't", [ ANSITerminal.yellow ])
            else ("\n" ^ user.name ^ " doesn't", [ ANSITerminal.red ])
          in
          ANSITerminal.print_string lst
            (nm ^ " have enough mana to use " ^ sk.name ^ "\n\n")
        in
        if List.length user.enem_hit_chances = 0 then (user, target, false)
        else (target, user, false)

let use_consumable csbl ch idx =
  let new_ch, _, did_action = use_skill csbl ch ch in
  if not did_action then (ch, false)
  else
    let new_ch' =
      if get_attribute_val "hp" new_ch > get_total_attr_val "maxhp" new_ch then
        { new_ch with hp = HP (get_total_attr_val "maxhp" new_ch) }
      else if
        get_attribute_val "mana" new_ch > get_total_attr_val "maxmana" new_ch
      then { new_ch with mana = Mana (get_total_attr_val "maxmana" new_ch) }
      else new_ch
    in
    let x = new_ch'.inv.(idx) in
    new_ch'.inv.(idx) <- { x with amt = x.amt - 1 };
    (new_ch', true)

let get_description_skill s = s.description
let get_description_item i = get_description_skill i

[@@@warning "-8"]
