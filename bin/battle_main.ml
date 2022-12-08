open Game
open Battle

let actor : Character.character = Character.start_character "Demoman"

let enem : Character.character =
  {
    name = "Marthia Pollocus";
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
    acc = Accuracy 10000.;
    mag = MagicPower 10.;
    luk = Luck 10.;
    enem_hit_chances = [ 1.; 0.25; 0.25 ];
    skillset = [| Some Character.demo_spell |];
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

let rec turn_handler (actor, enem) made_action =
  let actor, enem =
    if made_action then (Character.clear_temps actor, Character.clear_temps enem)
    else (actor, enem)
  in
  print_endline
    (actor.name ^ " HP: " ^ string_of_float (Character.get_attribute "hp" actor));
  print_endline
    (Character.get_name enem ^ " HP: "
    ^ string_of_float (Character.get_attribute "hp" enem));
  read_logo_files "data/menu.txt";
  match read_line () with
  | "attack" ->
      let first_turn_chars = (actor, Battle.attack enem actor) in
      let second_turn_chars = pick_enemy_move first_turn_chars in
      turn_handler second_turn_chars true
  | "guard" ->
      print_endline "You put your hands up and brace for an incoming attack.";
      let new_chars = pick_enemy_move (Battle.guard actor, enem) in
      turn_handler new_chars true
  | "escape" ->
      print_endline "You flee!";
      exit 0
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