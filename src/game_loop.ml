(** Represents the central game loop*)

open Tsdl
open Tsdl_image
open Components

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
  player : CornECS.entity;
  map : Tilemap.t;
}
(**The type of a value representing an instance of a game.*)

(**[set_spawn player m] sets the [player]'s spawn point on map [m].*)
let set_spawn player m =
  let spawn_x = Tilemap.get_spawn m |> fst in
  let spawn_y = (Tilemap.get_spawn m |> snd) - Constants.tilesize in
  Rectangle.set player
    ( fst (Rectangle.get player),
      Sdl.Rect.create
        (spawn_x * Tilemap.scale m)
        (spawn_y * Tilemap.scale m)
        Constants.tilesize Constants.tilesize );
  Position.set player (spawn_x * Tilemap.scale m, spawn_y * Tilemap.scale m);
  TargetPosition.set player
    (spawn_x * Tilemap.scale m, spawn_y * Tilemap.scale m)

(** 1. initialize the random seed for 2. initialize steps as a ref 0 3. randomly
    generate an encounter number 4. each time the player moves, incr steps 5.
    steps = encounter then make battle 6. battle ends return the new player
    character to the game loop *)

let rng () = 300 + (Random.int 5 * 100)

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
      let renderer = Sdl.create_renderer ~index:(-1) window |> Util.unwrap in
      Sdl.set_render_draw_color renderer 255 255 255 255 |> Util.unwrap;
      print_endline "Renderer created.";
      let map = Tilemap.load_map "data/cave.json" renderer in
      let player = Player.create_player "data/front.png" renderer in
      set_spawn player map;
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
        map;
      }
  | Error (`Msg err) -> failwith err

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

let get_item (arr : Character.consumable_bucket array) i =
  let dropped_amt = 1 + Random.int 4 in
  arr.(i) <- { (arr.(i)) with amt = arr.(i).amt + dropped_amt };
  ANSITerminal.print_string [ ANSITerminal.green ]
    ("Enemy dropped "
    ^
    if dropped_amt = 1 then arr.(i).name
    else string_of_int dropped_amt ^ " " ^ arr.(i).name ^ "s\n")

let drop_items actor =
  let arr = Character.get_inv actor in
  let rand = Random.float 1. in
  if rand <= 0.3 then get_item arr 0
  else if rand <= 0.5 then get_item arr 1
  else if rand <= 0.6 then get_item arr 2
  else if rand <= 0.7 then get_item arr 3
  else if rand <= 0.8 then get_item arr 4
  else get_item arr 5

let call_encounter a =
  let e = pick_enems () in
  let _ =
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      ("Encountered " ^ e.name ^ "!")
  in
  let _ = Battle_main.wait () in
  try Battle_main.start a e
  with Battle_main.Battle_Over a -> (
    if a = None then (
      ANSITerminal.print_string [ ANSITerminal.red ] "Game Over";
      exit 0)
    else
      match a with
      | Some ch ->
          let _ = drop_items ch in
          print_endline "Please return to the GUI";
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
let update game = Player.update game.player

(**[render game] first clears the renderer in the game instance [game], then
   renders all objects in said instance that need rendering.*)
let render game =
  Sdl.render_clear game.ren |> Util.unwrap;
  Tilemap.draw_map game.map game.ren;
  Player.draw game.player;
  Sdl.render_present game.ren

(**[clean game] "cleans" the game instance [game] by destroying the game window,
   the game renderer, and finally quitting.*)
let clean game =
  Sdl.quit_sub_system Sdl.Init.everything;
  Sdl.destroy_window game.win;
  Sdl.destroy_renderer game.ren;
  CornECS.delete game.player;

  Sdl.quit ();
  print_endline "Game cleaned."
