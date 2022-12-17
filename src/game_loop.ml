(** Represents the central game loop. *)

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
  (* musicplayer : Audio.t; *)
}
(**The type of a value representing an instance of a game.*)

(**[init t x y w h fs] creates a fresh game instance, in which the window has
   title [t], x-position [x], y-position [y], width [w], and height [h]. The
   game window will be fullscreen if [fs] is [true]. *)
let init t x y w h fs =
  (* let x = Sys.command "make battle" in *)
  let flags = if fs then Sdl.Window.fullscreen else Sdl.Window.windowed in
  Sdl.init Sdl.Init.everything |> Result.get_ok;
  (* let musicplayer = Audio.init () in *)
  let window = Sdl.create_window t ~x ~y ~w ~h flags |> Result.get_ok in
  (* Audio.load_music musicplayer (Constants.data_dir_prefix ^ "martha.wav") 2;
  Audio.load_music musicplayer (Constants.data_dir_prefix ^ "cavetheme.wav") 0;
  Audio.load_music musicplayer (Constants.data_dir_prefix ^ "buster.wav") 1; *)
  let renderer =
    Sdl.create_renderer ~index:(-1)
      ?flags:(Some Sdl.Renderer.(accelerated + presentvsync))
      window
    |> Result.get_ok
  in
  Sdl.set_render_draw_color renderer 255 255 255 255 |> Result.get_ok;
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
  let side =
    Textman.load_texture (Constants.data_dir_prefix ^ "side.png") renderer
  in
  let s =
    Textman.load_texture (Constants.data_dir_prefix ^ "front.png") renderer
  in
  Player.set_spawn player map;
  Player.init_anims player ~n ~e:side ~s ~w:side;
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
    (* musicplayer; *)
  }

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
  Sdl.render_clear game.ren |> Result.get_ok;
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
  Sdl.quit ()
