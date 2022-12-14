(** Represents the central game loop*)

open Tsdl
open Tsdl_image

type t = {
  title : string;
  xpos : int;
  ypos : int;
  w : int;
  h : int;
  fullscreen : bool;
  mutable running : bool;
  win : Sdl.window;
  ren : Sdl.renderer;
  cam : Camera.t;
  player : Player.t;
  map : Tilemap.t;
}
(**The type of a value representing an instance of a game.*)

(** [rng] is a function that generates a random number for encounters*)
let rng () = 5000 + Int.min (Random.int 10 * 100) (Random.int 10 * 100)

(**[init t x y w h fs] creates a fresh game instance, in which the window has
   title [t], x-position [x], y-position [y], width [w], and height [h]. The
   game window will be fullscreen if [fs] is [true]. *)
let init t x y w h fs =
  (* let x = Sys.command "make battle" in *)
  let flags = if fs then Sdl.Window.fullscreen else Sdl.Window.windowed in
  match Sdl.init Sdl.Init.everything with
  | Ok _ ->
      print_endline "Subsystems initialized.";
      let window = Sdl.create_window t ~x ~y ~w ~h flags |> Util.unwrap in
      print_endline "Window created.";

      let renderer =
        Sdl.create_renderer ~index:(-1)
          ?flags:(Some Sdl.Renderer.(accelerated + presentvsync))
          window
        |> Util.unwrap
      in
      Sdl.set_render_draw_color renderer 255 255 255 255 |> Util.unwrap;
      print_endline "Renderer created.";
      let map =
        Tilemap.load_map (Constants.data_dir_prefix ^ "cave.json") renderer
      in
      let player =
        Player.create_player (Constants.data_dir_prefix ^ "front.png") renderer
      in
      let cam = Camera.create_camera player in
      let n =
        Textman.load_texture (Constants.data_dir_prefix ^ "back.png") renderer
      in
      let e =
        Textman.load_texture (Constants.data_dir_prefix ^ "side.png") renderer
      in
      let w_tex =
        Textman.load_texture (Constants.data_dir_prefix ^ "side.png") renderer
      in
      let s =
        Textman.load_texture (Constants.data_dir_prefix ^ "front.png") renderer
      in
      Player.set_spawn player map;
      Player.init_anims player ~n ~e ~s ~w:w_tex;
      {
        title = t;
        xpos = x;
        ypos = y;
        w;
        h;
        running = true;
        fullscreen = fs;
        win = window;
        ren = renderer;
        player;
        cam;
        map;
      }
  | Error (`Msg err) -> failwith err

(** [pick_enems] randomly picks a character from characters.json to battle*)
let pick_enems () =
  let arr =
    [|
      Character.parse_character "Clocktower Fiend" [ 0.7; 0.3; 0.; 0.; 0.; 0. ];
      Character.parse_character "Hovian Plaza Serpent"
        [ 0.2; 0.0; 0.2; 0.4; 0.1; 0.1 ];
      Character.parse_character "Gorge Gorgon" [ 0.2; 0.1; 0.2; 0.2; 0.1; 0.2 ];
    |]
  in
  let rand = Random.float 1. in
  let e =
    if rand < 0.4 then arr.(0) else if rand < 0.8 then arr.(1) else arr.(2)
  in
  e

(**[get_item arr -> int -> unit] takes in a character [actor]'s consumable bucket array
   and increases the consumable at index [i] by a random amount between 1 and 5*)
let get_item (arr : Character.consumable_bucket array) i =
  let dropped_amt = 1 + Random.int 4 in
  arr.(i) <- { (arr.(i)) with amt = arr.(i).amt + dropped_amt };
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("Enemy dropped "
    ^
    if dropped_amt = 1 then arr.(i).name
    else string_of_int dropped_amt ^ " " ^ arr.(i).name ^ "s\n")
(**[drop_items actor -> character -> unit] takes in a character [actor] and randomly chooses a consumable to increase*)
let drop_items actor =
  let arr = Character.get_inv actor in
  let rand = Random.float 1. in
  if rand <= 0.3 then get_item arr 0
  else if rand <= 0.5 then get_item arr 1
  else if rand <= 0.6 then get_item arr 2
  else if rand <= 0.7 then get_item arr 3
  else if rand <= 0.8 then get_item arr 4
  else get_item arr 5

(** [call_encounter actor -> actor] takes in a character and initiates a battle. Returns another character once the battle is over*)
let call_encounter a =
  let e = pick_enems () in
  let _ =
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      ("Encountered " ^ e.name ^ "!")
  in
  let _ = Battle_handler.wait () in
  try Battle_handler.start a e
  with Battle_handler.Battle_Over a -> (
    if a = None then (
      ANSITerminal.print_string [ ANSITerminal.red ] "\nGame Over";
      exit 0)
    else
      match a with
      | Some ch ->
          let _ = drop_items ch in
          print_endline "Please return to the GUI";
          ch
      | None -> failwith "Not reachable")

(**[boss_battle a : character -> character] calls a boss battle with player character [a] and returns the modified player character after the battle is over.*)
let boss_battle a =
  let boss =
    Character.parse_character "Marthia Pollocus"
      [ 0.2; 0.1; 0.3; 0.3; 0.1; 0.1 ]
  in
  ANSITerminal.print_string [ ANSITerminal.red ]
    "Encountered Martha Pollocus and the Weather Machine!";
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "You feel like you're going to have a bad 4 years.";
  let _ = Battle_handler.wait () in
  try Battle_handler.start a boss
  with Battle_handler.Battle_Over a -> (
    if a = None then (
      ANSITerminal.print_string [ ANSITerminal.red ] "\nGame Over\n\n";
      exit 0)
    else
      match a with
      | Some ch ->
          let _ = drop_items ch in
          ANSITerminal.print_string [ ANSITerminal.green ]
            "\nCongratulations, You have won!";
          ch
      | None -> failwith "Not reachable")

(**[handle_events game] handles events of the game instance [game].*)
let handle_events game =
  let e = Sdl.Event.create () in
  while Sdl.poll_event (Some e) do
    match Sdl.Event.(enum (get e typ)) with
    | `Quit -> game.running <- false
    | _ -> ()
  done;
  Player.handle game.player

(**[update game] updates all objects in the game instance [game] that need
   updating.*)
let update game =
  Camera.track game.cam game.player;
  Player.update game.player game.map

(**[render game] first clears the renderer in the game instance [game], then
   renders all objects in said instance that need rendering.*)
let render game =
  Sdl.render_clear game.ren |> Util.unwrap;
  Tilemap.draw_map game.map game.ren game.cam;
  Player.draw game.player game.cam;
  Sdl.render_present game.ren

(**[clean game] "cleans" the game instance [game] by destroying the game window,
   the game renderer, and finally quitting.*)
let clean game =
  Sdl.quit_sub_system Sdl.Init.everything;
  Sdl.destroy_window game.win;
  Sdl.destroy_renderer game.ren;
  Components.CornECS.delete game.player;
  Camera.delete game.cam;

  Sdl.quit ();
  print_endline "Game cleaned."