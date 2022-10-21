(* open Game

(**[listen adv st] prompts for commands of type [Command.command], parses them,
   and attempts to update the game state [st] if the command leads to a valid
   location. The user will be prompted for input again if the command was
   invalid, or if the exit specified was invalid. *)
let rec listen adv st =
  print_endline "Where would you like to go next?";
  print_string "> ";
  try
    match read_line () |> Command.parse with
    | Go dir ->
        print_endline "Temp for testing Thanks for playing.";
        exit 0
        (* ( match State.go dir adv st with | Legal lg -> let new_room =
           State.current_room_id lg in let new_room_desc = Adventure.description
           adv new_room in if List.mem new_room (State.visited st) then
           print_endline ("This place seems familiar. " ^ new_room_desc) else
           print_endline new_room_desc; listen adv lg | Illegal -> prompt ("You
           cannot exit this room from the " ^ Game_fmts.dir_to_string dir ^ ".
           Please try again.") adv st) *)
    | Quit ->
        print_endline "Thanks for playing.";
        exit 0
    | Fight ft -> exit 0
    (*TODO: implement battle behavior once [battle.ml] is done. Aadarsh uwu*)
  with
  | Command.Malformed ->
      prompt "Command not understood. Please try again." adv st
  | Command.Empty ->
      prompt "You didn't enter anything! Please try again." adv st

(**[prompt str adv st] prompts the user with [str], then listens for a command.*)
and prompt str adv st =
  print_endline str;
  listen adv st

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  let adv = f |> Yojson.Basic.from_file |> Adventure.from_json in
  let st = State.init_state adv in
  listen adv st

let data_dir_prefix = "data" ^ Filename.dir_sep

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> (
      let file = data_dir_prefix ^ file_name ^ ".json" in
      try if Sys.file_exists file then play_game file else raise Not_found
      with Not_found ->
        ANSITerminal.prerr_string [ ANSITerminal.cyan ]
          "File not found, terminating.\n")

(* Execute the game engine. *)
let () = main () *)
