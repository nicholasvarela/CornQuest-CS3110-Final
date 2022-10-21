(* open Game *)

(* (**[listen adv st] prompts for commands of type [Command.command], parses
   them, and attempts to update the game state [st] if the command leads to a
   valid location. The user will be prompted for input again if the command was
   invalid, or if the exit specified was invalid. *) let rec listen adv st =
   print_endline "Where would you like to go next?"; print_string "> "; try
   match read_line () |> Command.parse with | Go dir -> print_endline "Temp for
   testing Thanks for playing."; exit 0 (* ( match State.go dir adv st with |
   Legal lg -> let new_room = State.current_room_id lg in let new_room_desc =
   Adventure.description adv new_room in if List.mem new_room (State.visited st)
   then print_endline ("This place seems familiar. " ^ new_room_desc) else
   print_endline new_room_desc; listen adv lg | Illegal -> prompt ("You cannot
   exit this room from the " ^ Game_fmts.dir_to_string dir ^ ". Please try
   again.") adv st) *) | Quit -> print_endline "Thanks for playing."; exit 0 |
   Fight ft -> exit 0 (*TODO: implement battle behavior once [battle.ml] is
   done. Aadarsh uwu*) with | Command.Malformed -> prompt "Command not
   understood. Please try again." adv st | Command.Empty -> prompt "You didn't
   enter anything! Please try again." adv st *)

(* *[prompt str adv st] prompts the user with [str], then listens for a command.
   and prompt str adv st = print_endline str; listen adv st

   (** [play_game f] starts the adventure in file [f]. *) let play_game f = let
   adv = f |> Yojson.Basic.from_file |> Adventure.from_json in let st =
   State.init_state adv in listen adv st *)

(* let data_dir_prefix = "data" ^ Filename.dir_sep *)

(** Code for file input is adapted from standard library and other credited
    sources https://ocaml.org/docs/file-manipulation
    https://stackoverflow.com/questions/5774934/how-do-i-read-in-lines-from-a-text-file-in-ocaml*)
let read_logo_files filename =
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  let listener = open_in filename in
  try
    while true do
      print_endline (input_line listener)
    done
  with End_of_file -> print_endline ""

let start_game filename =
  print_endline filename;
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.green ] "\n\nWelcome to CornQuest!\n";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "You are currently in the far away land of Ithaca, New York";
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "It is fall and the leaves have just started to change color";
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.green ]
    "There has been reports of a enemies in Rhodes Hall, go check it out...";
  print_endline "";
  ANSITerminal.print_string [ ANSITerminal.green ] "Be careful out there..";
  print_endline "";
  print_endline "";
  print_endline "";
  print_endline "";

  print_endline "You are in Rhodes Hall, feel free to explore ";
  print_endline "";
  print_endline "";
  print_endline ""

(**Start Game Now*)

let main () =
  read_logo_files "data/cornlogo1.txt";
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n\nWelcome to CornQuest!\n";
  print_endline "Please enter your characters name\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> start_game file_name

(* Execute the game engine. *)
let () = main ()
