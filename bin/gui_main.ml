open Tsdl
open Game

let in_battle = ref false
let ch = ref (Character.start_character "Huy")

let main () =
  Random.self_init ();
  let fps = 60 in
  let frame_delay = 1000 / fps in
  let frame_start = ref 0 in
  let frame_time = ref 0 in
  let game =
    Game_loop.init "bruh" Sdl.Window.pos_centered Sdl.Window.pos_centered
      Constants.screen_width Constants.screen_height false
  in
  Driver.read_logo_files "data/title.txt";
  let encounter = ref (Game_loop.rng ()) in
  while game.running do
    frame_start := Int32.to_int (Sdl.get_ticks ());
    Game_loop.handle_events game;
    Game_loop.update game;
    Game_loop.render game;
    frame_time := Int32.to_int (Sdl.get_ticks ()) - !frame_start;
    if frame_delay > !frame_time then
      Sdl.delay (Int32.of_int (frame_delay - !frame_time));

    if !in_battle = false then
      if !Player.steps >= !encounter then (
        in_battle := true;
        let i = ref true in
        while !i do
          ch := Game_loop.call_encounter !ch;
          i := false;
          Player.steps := 0
        done;
        encounter := Game_loop.rng ();
        in_battle := false)
  done;
  Game_loop.clean game

let () = main ()