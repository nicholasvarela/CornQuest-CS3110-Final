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

(**[create_player fl ren] creates a player entity with textures loaded from the
   file [fl] using the renderer [ren].*)
let create_player fl ren =
  CornECS.next_id () |> Sprite.b |> KeyboardController.b
  |> Renderable.s (Textman.load_texture fl ren, ren)
  |> Position.s (0, 0)

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
  Position.set player (spawn_x * Tilemap.scale m, spawn_y * Tilemap.scale m)

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
      let player = create_player "data/front.png" renderer in
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

(**[handle_events game] handles events of the game instance [game].*)
let handle_events game =
  let e = Sdl.Event.create () in
  while Sdl.poll_event (Some e) do
    KeyboardController.handle e game.player;
    match Sdl.Event.(enum (get e typ)) with
    | `Quit -> game.running <- false
    | _ -> ()
  done

(**[update game] updates all objects in the game instance [game] that need
   updating.*)
let update game = KeyboardController.update game.player

(**[render game] first clears the renderer in the game instance [game], then
   renders all objects in said instance that need rendering.*)
let render game =
  Sdl.render_clear game.ren |> Util.unwrap;
  Tilemap.draw_map game.map game.ren;
  Sprite.draw game.player;
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
