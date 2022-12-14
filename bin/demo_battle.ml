open Game
(** [main ()] prompts for the game to play, then starts it. *)
let actor : Character.character = Character.start_character "Demoman"

let enem : Character.character = Character.parse_character "Marthia Pollocus and the Weather Machine" [ 0.2; 0.0; 0.2; 0.4; 0.1; 0.1 ]

let main () = Battle_handler.start actor enem 

(* print_endline "Please enter the name of the game file you want to load.\n";
   print_string "> "; match read_line () with | exception End_of_file -> () |
   file_name -> ( let file = data_dir_prefix ^ file_name ^ ".json" in try if
   Sys.file_exists file then play_game file else raise Not_found with Not_found
   -> ANSITerminal.prerr_string [ ANSITerminal.cyan ] "File not found,
   terminating.\n") *)

(* Execute the game engine. *)
let () = main ()