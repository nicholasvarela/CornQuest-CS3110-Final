open Game
open Battle
open Character

exception Battle_Over of Character.character option

let actor : Character.character = Character.start_character "Demoman"

let enem : Character.character =
  {
    name = "Marthia Pollocus";
    hp = HP 10.;
    maxhp = HP 100.;
    mana = Mana 100.;
    maxmana = Mana 100.;
    exp = 25;
    lvl = 1;
    str = Strength 10.;
    def = Defense 10.;
    mr = MagicResist 10.;
    spd = Speed 2.;
    acc = Accuracy 10.;
    mag = MagicPower 10.;
    luk = Luck 10.;
    enem_hit_chances = [ 0.5; 0.25; 0.25 ];
    skillset = [| Some Character.icicle |];
    inv = [||];
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

let read_logo_files filename =
  let listener = open_in filename in
  try
    while true do
      print_endline (input_line listener)
    done
  with End_of_file -> print_endline ""

let data_dir_prefix = "data" ^ Filename.dir_sep
let dots = "......................................."

let print_skills actor =
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

let print_items actor =
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
  if Character.get_attribute_val "hp" enem <= 0. then
    let _ = print_endline (enem.name ^ " was defeated!") in
    let experienced_char = { actor with exp = actor.exp + enem.exp } in
    let out =
      if experienced_char.exp mod 25 = 0 then level_up experienced_char
      else experienced_char
    in
    raise (Battle_Over (Some out))
  else if Character.get_attribute_val "hp" actor <= 0. then
    let _ = print_endline (actor.name ^ " was defeated!") in
    raise (Battle_Over None)
  else (actor, enem)

and item_menu (actor, enem) =
  let arr = Character.get_inv actor in
  let bk1 = if arr.(0).amt > 0 then arr.(0).name else "" in
  let bk2 = if arr.(1).amt > 0 then arr.(1).name else "" in
  let bk3 = if arr.(2).amt > 0 then arr.(2).name else "" in
  let bk4 = if arr.(3).amt > 0 then arr.(3).name else "" in
  let bk5 = if arr.(4).amt > 0 then arr.(4).name else "" in
  let bk6 = if arr.(5).amt > 0 then arr.(5).name else "" in
  let inp = read_line () in
  match inp with
  | s when String.length s = 0 -> item_menu (actor, enem)
  | s when inp = bk1 ->
      let a = Character.use_consumable arr.(0).item actor 0 in
      let a2, e2 = Battle.pick_enemy_move (a, enem) in
      let a2', e2' = check_health (a2, e2) in
      turn_handler (a2', e2') true
  | s when inp = bk2 ->
      let a = Character.use_consumable arr.(1).item actor 1 in
      let a2, e2 = Battle.pick_enemy_move (a, enem) in
      let a2', e2' = check_health (a2, e2) in
      turn_handler (a2', e2') true
  | s when inp = bk3 ->
      let a = Character.use_consumable arr.(2).item actor 2 in
      let a2, e2 = Battle.pick_enemy_move (a, enem) in
      let a2', e2' = check_health (a2, e2) in
      turn_handler (a2', e2') true
  | s when inp = bk4 ->
      let a = Character.use_consumable arr.(3).item actor 3 in
      let a2, e2 = Battle.pick_enemy_move (a, enem) in
      let a2', e2' = check_health (a2, e2) in
      turn_handler (a2', e2') true
  | s when inp = bk5 ->
      let a = Character.use_consumable arr.(4).item actor 4 in
      let a2, e2 = Battle.pick_enemy_move (a, enem) in
      let a2', e2' = check_health (a2, e2) in
      turn_handler (a2', e2') true
  | s when inp = bk6 ->
      let a = Character.use_consumable arr.(5).item actor 5 in
      let a2, e2 = Battle.pick_enemy_move (a, enem) in
      let a2', e2' = check_health (a2, e2) in
      turn_handler (a2', e2') true
  | "back" -> turn_handler (actor, enem) false
  | _ -> item_menu (actor, enem)

and skill_menu (actor, enem) =
  let arr = Character.get_skills actor in
  let sk1 =
    try (Character.unwrap_skill arr.(0)).name with Character.No_skill -> ""
  in
  let sk2 =
    try (Character.unwrap_skill arr.(1)).name with Character.No_skill -> ""
  in
  let sk3 =
    try (Character.unwrap_skill arr.(2)).name with Character.No_skill -> ""
  in
  let sk4 =
    try (Character.unwrap_skill arr.(3)).name with Character.No_skill -> ""
  in
  let inp = read_line () in
  match inp with
  | s when String.length s = 0 -> skill_menu (actor, enem)
  | s when inp = sk1 ->
      let a, e, b =
        Character.use_skill (Character.unwrap_skill arr.(0)) actor enem
      in
      let a', e' = check_health (a, e) in
      if b then
        let a2, e2 = Battle.pick_enemy_move (a', e') in
        let a2', e2' = check_health (a2, e2) in
        turn_handler (a2', e2') true
      else skill_menu (a', e')
  | s when inp = sk2 ->
      let a, e, b =
        Character.use_skill (Character.unwrap_skill arr.(1)) actor enem
      in
      let a', e' = check_health (a, e) in
      if b then
        let a2, e2 = Battle.pick_enemy_move (a', e') in
        let a2', e2' = check_health (a2, e2) in
        turn_handler (a2', e2') true
      else skill_menu (a', e')
  | s when inp = sk3 ->
      let a, e, b =
        Character.use_skill (Character.unwrap_skill arr.(3)) actor enem
      in
      let a', e' = check_health (a, e) in
      if b then
        let a2, e2 = Battle.pick_enemy_move (a', e') in
        let a2', e2' = check_health (a2, e2) in
        turn_handler (a2', e2') true
      else skill_menu (a', e')
  | s when inp = sk4 ->
      let a, e, b =
        Character.use_skill (Character.unwrap_skill arr.(4)) actor enem
      in
      let a', e' = check_health (a, e) in
      if b then
        let a2, e2 = Battle.pick_enemy_move (a', e') in
        let a2', e2' = check_health (a2, e2) in
        turn_handler (a2', e2') true
      else skill_menu (a', e')
  | "back" -> turn_handler (actor, enem) false
  | _ -> skill_menu (actor, enem)

and turn_handler (actor, enem) made_action =
  let actor, enem =
    if made_action then (Character.clear_temps actor, Character.clear_temps enem)
    else (actor, enem)
  in
  print_endline
    (actor.name ^ " HP: "
    ^ string_of_int (int_of_float (Character.get_attribute_val "hp" actor))
    ^ " Mana: "
    ^ string_of_int (int_of_float (Character.get_attribute_val "mana" actor)));
  print_endline
    (Character.get_name enem ^ " HP: "
    ^ string_of_int (int_of_float (Character.get_attribute_val "hp" enem)));
  read_logo_files "data/menu.txt";
  match read_line () with
  | "attack" ->
      print_string (actor.name ^ " attacked!");
      let first_turn_chars = (actor, Battle.attack enem actor) in
      let _ = check_health first_turn_chars in
      let second_turn_chars = pick_enemy_move first_turn_chars in
      let _ = check_health second_turn_chars in
      turn_handler second_turn_chars true
  | "guard" ->
      print_endline "You put your hands up and brace for an incoming attack.";
      let new_chars = pick_enemy_move (Battle.guard actor, enem) in
      let _ = check_health new_chars in
      turn_handler new_chars true
  | "skill" ->
      print_skills actor;
      skill_menu (actor, enem)
  | "escape" ->
      print_endline "You flee!";
      exit 0
  | "item" ->
      print_items actor;
      item_menu (actor, enem)
  | _ -> turn_handler (actor, enem) false

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  read_logo_files "data/title.txt";
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWhat will you do?\n";
  turn_handler (actor, enem) false

(* print_endline "Please enter the name of the game file you want to load.\n";
   print_string "> "; match read_line () with | exception End_of_file -> () |
   file_name -> ( let file = data_dir_prefix ^ file_name ^ ".json" in try if
   Sys.file_exists file then play_game file else raise Not_found with Not_found
   -> ANSITerminal.prerr_string [ ANSITerminal.cyan ] "File not found,
   terminating.\n") *)

(* Execute the game engine. *)
let () = main ()