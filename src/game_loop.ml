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
  player : Gameobj.t;
  player_ecs : CornECS.entity;
  enemy : Gameobj.t;
  map : Tilemap.t;
}
(**The type of a value representing an instance of a game.*)

let create_player tex ren =
  CornECS.next_id () |> Sprite.b |> Renderable.s (tex, ren)

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
      let enemy = Gameobj.create "data/xande.png" renderer 0 0 in
      let player = Gameobj.create "data/cloud.png" renderer 900 900 in

      let player_ecs =
        create_player (Textman.load_texture "data/kefka.png" renderer) renderer
      in
      let map = Tilemap.load_map "data/ff3img.png" "data/cave.json" renderer in
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
        player_ecs;
        enemy;
        map;
      }
  | Error (`Msg err) -> failwith err

(**[handle_events game] handles events of the game instance [game].*)
let handle_events game =
  let e = Sdl.Event.create () in
  let _ = Sdl.poll_event (Some e) in
  match Sdl.Event.(enum (get e typ)) with
  | `Quit -> game.running <- false
  | _ -> ()

(**[update game] updates all objects in the game instance [game] that need
   updating.*)
let update game =
  Gameobj.update game.player (-1) (-1);
  Sprite.update game.player_ecs 1 1;
  Gameobj.update game.enemy 1 1;
  ()

(**[render game] first clears the renderer in the game instance [game], then
   renders all objects in said instance that need rendering.*)
let render game =
  Sdl.render_clear game.ren |> Util.unwrap;
  Tilemap.draw_map game.map game.ren;
  Gameobj.render game.player game.ren |> Util.unwrap;
  Gameobj.render game.enemy game.ren |> Util.unwrap;
  Sprite.draw game.player_ecs;
  Sdl.render_present game.ren

(**[clean game] "cleans" the game instance [game] by destroying the game window,
   the game renderer, and finally quitting.*)
let clean game =
  Sdl.destroy_window game.win;
  Sdl.destroy_renderer game.ren;
  Sdl.quit ();
  print_endline "Game cleaned."
