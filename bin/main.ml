open Tsdl
open Game

let in_battle = ref false
let ch = ref (Character.start_character "Huy")
let do_boss_battle = ref false

let main () =
  if !ch.lvl = 6 then do_boss_battle := true;
  Random.self_init ();
  let fps = 60 in
  let frame_delay = 1000 / fps in
  let frame_start = ref 0 in
  let frame_time = ref 0 in
  let game =
    Game_loop.init "CornQuest" Sdl.Window.pos_centered Sdl.Window.pos_centered
      Constants.screen_width Constants.screen_height false
  in
  Battle_handler.read_logo_files "data/title.txt";
  print_endline "For the best experience, open the terminal in fullscreen!";
  let encounter = ref (Game_loop.rng ()) in
  Audio.play game.musicplayer 0;
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
        let f =
          if !do_boss_battle = true then (
            Audio.play game.musicplayer 2;
            Game_loop.boss_battle)
          else (
            Audio.play game.musicplayer 1;
            Game_loop.call_encounter)
        in
        in_battle := true;
        let i = ref true in
        while !i do
          ch := f !ch;
          i := false;
          Player.steps := 0
        done;
        encounter := Game_loop.rng ();
        in_battle := false;
        Audio.play game.musicplayer 0)
  done;
  Game_loop.clean game

let () = main ()