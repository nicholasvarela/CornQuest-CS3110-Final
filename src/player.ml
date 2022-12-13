open Components
open Tsdl

let steps = ref 0

let create_player fl ren =
  CornECS.next_id () |> Sprite.b |> KeyboardController.b
  |> Renderable.s (Textman.load_texture fl ren, ren)
  |> Position.s (0, 0)
  |> TargetPosition.s (0, 0)

let draw e =
  let tex, ren = Renderable.get e in
  let src, dst = Rectangle.get e in
  ignore (Sdl.render_copy ~src ~dst ren tex)

(**[update_vel e x y] updates the [Velocity] of entity [e], making its
   x-directional speed [x] and its y-directional speed [y].*)
let update_vel e x y =
  let xpos, ypos = Position.get e in
  let dst = snd (Rectangle.get e) in
  Velocity.set e (x, y);
  Sdl.Rect.set_x dst xpos;
  Sdl.Rect.set_y dst ypos

(**[update_pos e] updates the [Position] of entity [e] based on its [Velocity].*)
let update_pos e =
  let posx, posy = Position.get e in
  let velx, vely = Velocity.get e in
  Position.set e (posx + velx, posy + vely)

let handle e =
  let target = TargetPosition.get e in
  if (Keyboard.get e).{Sdl.Scancode.w} = 1 && not (Moving.get e) then (
    Moving.set e true;
    Direction.set e North;
    TargetPosition.set e (fst target, snd target - Constants.tilesize));
  if (Keyboard.get e).{Sdl.Scancode.a} = 1 && not (Moving.get e) then (
    Moving.set e true;
    Direction.set e West;
    TargetPosition.set e (fst target - Constants.tilesize, snd target));
  if (Keyboard.get e).{Sdl.Scancode.s} = 1 && not (Moving.get e) then (
    Moving.set e true;
    Direction.set e South;
    TargetPosition.set e (fst target, snd target + Constants.tilesize));
  if (Keyboard.get e).{Sdl.Scancode.d} = 1 && not (Moving.get e) then (
    Moving.set e true;
    Direction.set e East;
    TargetPosition.set e (fst target + Constants.tilesize, snd target))

let update e =
  let xtar, ytar = TargetPosition.get e in
  let xpos, ypos = Position.get e in
  if TargetPosition.get e = Position.get e then (
    Moving.set e false;
    update_vel e 0 0);
  if xtar > xpos then update_vel e Constants.player_speed 0;
  if xtar < xpos then update_vel e (-Constants.player_speed) 0;
  if ytar > ypos then update_vel e 0 Constants.player_speed;
  if ytar < ypos then update_vel e 0 (-Constants.player_speed);
  update_pos e;
  steps := !steps + Int.abs (xtar - xpos) + Int.abs (ytar - ypos)
