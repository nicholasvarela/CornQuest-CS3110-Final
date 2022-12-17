open Battle
open Character

exception Battle_Over of Character.character option

let first_time_skill = ref true
let first_time_item = ref true

let read_logo_files filename =
  let listener = open_in filename in
  print_newline ();
  try
    while true do
      print_endline (input_line listener)
    done
  with End_of_file -> print_newline ()

let data_dir_prefix = "data" ^ Filename.dir_sep
let dots = "......................................."

let print_skills actor first_time =
  if !first_time then (
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "[type [skill name] (no caps) to use it, [info + skill name]\n\
       to read what the skill does, or [back] to exit the menu]\n";
    first_time := false);
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\n[..................Skill Menu................]\n";
  let arr = Character.get_skills actor in
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

let get_bk_names arr =
  let a = if arr.(0).amt > 0 then arr.(0).name else "" in
  let b = if arr.(1).amt > 0 then arr.(1).name else "" in
  let c = if arr.(2).amt > 0 then arr.(2).name else "" in
  let d = if arr.(3).amt > 0 then arr.(3).name else "" in
  let e = if arr.(4).amt > 0 then arr.(4).name else "" in
  let f = if arr.(5).amt > 0 then arr.(5).name else "" in
  (a, b, c, d, e, f)

let get_sk_names arr =
  let a =
    try (Character.unwrap_skill arr.(0)).name with Character.No_skill -> ""
  in
  let b =
    try (Character.unwrap_skill arr.(1)).name with Character.No_skill -> ""
  in
  let c =
    try (Character.unwrap_skill arr.(2)).name with Character.No_skill -> ""
  in
  let d =
    try (Character.unwrap_skill arr.(3)).name with Character.No_skill -> ""
  in
  (a, b, c, d)

let wait () =
  let i = ref 0 in
  while !i < 100 do
    match read_line () with
    | _ -> i := 100
  done

let print_items actor first_time =
  if !first_time then (
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "\n\
       [type [item name] to use an item, [info item name] to \n\
       read what the skill does, or [back] to exit the menu]\n";
    first_time := false;
    let a = wait () in
    a);
  let _ =
    ANSITerminal.print_string [ ANSITerminal.cyan ]
      "\n[...............Skill Menu..............]\n"
  in
  let arr = Character.get_inv actor in
  let cnt = ref 6 in
  for i = 0 to Array.length arr - 1 do
    let bk = arr.(i) in
    if arr.(i).amt > 0 then (
      cnt := !cnt - 1;
      let s = String.capitalize_ascii bk.name in
      let minus =
        if bk.amt > 99 then String.length s + 1 else String.length s
      in
      print_endline
        ("● " ^ s ^ String.sub dots 0 (38 - minus) ^ string_of_int bk.amt))
  done;
  for i = 0 to !cnt - 1 do
    print_endline ("● " ^ dots)
  done

let rec check_health (actor, enem) =
  if Character.get_attribute_val "hp" enem < 1. then
    let _ = print_endline (enem.name ^ " was defeated!") in
    let experienced_char = { actor with exp = actor.exp + enem.exp } in
    let out =
      if experienced_char.exp >= 50 then level_up experienced_char
      else experienced_char
    in
    raise (Battle_Over (Some out))
  else if Character.get_attribute_val "hp" actor <= 0. then
    let _ = print_endline (actor.name ^ " was defeated!") in
    raise (Battle_Over None)
  else (actor, enem)

and item_menu_helper (actor, enem) arr i =
  let a, did_action = Character.use_consumable arr.(i).item actor i in
  if not did_action then item_menu (actor, enem)
  else
    let e' = Character.clear_temps (ref enem) in
    let a2, e2 = Battle.pick_enemy_move (a, e') in
    let a2', e2' = check_health (a2, e2) in
    turn_handler (a2', e2') true

and item_info_helper (actor, enem) arr i =
  ANSITerminal.print_string [ ANSITerminal.default ]
    (get_description_item arr.(i).item ^ "\n");
  item_menu (actor, enem)

and item_menu (actor, enem) =
  let arr = Character.get_inv actor in
  let bk1, bk2, bk3, bk4, bk5, bk6 = get_bk_names arr in
  let inp = read_line () in
  match inp with
  | s when String.length s = 0 -> item_menu (actor, enem)
  | s when inp = bk1 -> item_menu_helper (actor, enem) arr 0
  | s when inp = bk2 -> item_menu_helper (actor, enem) arr 1
  | s when inp = bk3 -> item_menu_helper (actor, enem) arr 2
  | s when inp = bk4 -> item_menu_helper (actor, enem) arr 3
  | s when inp = bk5 -> item_menu_helper (actor, enem) arr 4
  | s when inp = bk6 -> item_menu_helper (actor, enem) arr 5
  | s when s = "info " ^ bk1 -> item_info_helper (actor, enem) arr 0
  | s when s = "info " ^ bk2 -> item_info_helper (actor, enem) arr 1
  | s when s = "info " ^ bk3 -> item_info_helper (actor, enem) arr 2
  | s when s = "info " ^ bk4 -> item_info_helper (actor, enem) arr 3
  | s when s = "info " ^ bk5 -> item_info_helper (actor, enem) arr 4
  | s when s = "info " ^ bk6 -> item_info_helper (actor, enem) arr 5
  | "back" -> turn_handler (actor, enem) false
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "That's not a valid item. Please try again\n";
      item_menu (actor, enem)

and skill_menu_helper (actor, enem) arr i =
  let a, e, b =
    Character.use_skill (Character.unwrap_skill arr.(i)) actor enem
  in
  let a', e' = check_health (a, e) in
  if b then
    let e'' = Character.clear_temps (ref e') in
    let a2, e2 = Battle.pick_enemy_move (a', e'') in
    let a2', e2' = check_health (a2, e2) in
    turn_handler (a2', e2') true
  else skill_menu (a', e')

and skill_info_helper (actor, enem) arr i =
  ANSITerminal.print_string [ ANSITerminal.default ]
    ((Character.unwrap_skill arr.(i)).description ^ "\n");
  skill_menu (actor, enem)

and skill_menu (actor, enem) =
  let arr = Character.get_skills actor in
  let sk1, sk2, sk3, sk4 = get_sk_names arr in
  let inp = read_line () in
  match inp with
  | s when String.length s = 0 -> skill_menu (actor, enem)
  | s when inp = sk1 -> skill_menu_helper (actor, enem) arr 0
  | s when inp = sk2 -> skill_menu_helper (actor, enem) arr 1
  | s when inp = sk3 -> skill_menu_helper (actor, enem) arr 2
  | s when inp = sk4 -> skill_menu_helper (actor, enem) arr 3
  | "back" -> turn_handler (actor, enem) false
  | s when s = "info " ^ sk1 -> skill_info_helper (actor, enem) arr 0
  | s when s = "info " ^ sk2 -> skill_info_helper (actor, enem) arr 1
  | s when s = "info " ^ sk3 -> skill_info_helper (actor, enem) arr 2
  | s when s = "info " ^ sk4 -> skill_info_helper (actor, enem) arr 3
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ]
        "That's not a valid skill. Please try again\n";
      skill_menu (actor, enem)

and turn_handler (actor, enem) made_action =
  ANSITerminal.print_string [ ANSITerminal.white ]
    (actor.name ^ " HP: "
    ^ string_of_int (int_of_float (Character.get_attribute_val "hp" actor))
    ^ " Mana: "
    ^ string_of_int (int_of_float (Character.get_attribute_val "mana" actor))
    ^ "\n");

  ANSITerminal.print_string [ ANSITerminal.red ]
    (Character.get_name enem ^ " HP: "
    ^ string_of_int (int_of_float (Character.get_attribute_val "hp" enem))
    ^ "\n");
  read_logo_files "data/menu.txt";
  match read_line () with
  | "attack" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ("\n" ^ actor.name ^ " attacked!");
      let a, e = (actor, Battle.attack enem actor) in
      let _ = check_health (a, e) in
      let e' = Character.clear_temps (ref e) in
      let second_turn_chars = pick_enemy_move (a, e') in
      let _ = check_health second_turn_chars in
      turn_handler second_turn_chars true
  | "guard" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "\nYou brace for an incoming attack.\n";
      wait ();
      let e' = Character.clear_temps (ref enem) in
      let new_chars = pick_enemy_move (Battle.guard actor, e') in
      let _ = check_health new_chars in
      turn_handler new_chars true
  | "skill" ->
      print_skills actor first_time_skill;
      skill_menu (actor, enem)
  | "escape" ->
      if enem.name = "Marthia Pollocus" then
        let _ =
          ANSITerminal.print_string [ ANSITerminal.yellow ]
            "It's a boss battle! You can't escape!\n"
        in
        turn_handler (actor, enem) false
      else
        let _ = print_endline "You flee! Please return to the GUI." in
        raise (Battle_Over (Some actor))
  | "item" ->
      print_items actor first_time_item;
      item_menu (actor, enem)
  | _ -> turn_handler (actor, enem) false

let start a e =
  let enem_art =
    match Character.get_name e with
    | "Clocktower Fiend" -> "clockfiend.txt"
    | "Hovian Plaza Serpent" -> "hoserp.txt"
    | "Gorge Gorgon" -> "gorgegon.txt"
    | "Marthia Pollocus" -> "martha.txt"
    | _ -> failwith "unreachable"
  in
  read_logo_files
    (Constants.data_dir_prefix ^ Filename.dir_sep ^ "ascii" ^ Filename.dir_sep
   ^ enem_art);
  ANSITerminal.print_string [ ANSITerminal.yellow ]
    "\n\n\
     Choose a move: attack, guard, skill, item or escape. What will you do? \n\n";
  turn_handler (a, e) false