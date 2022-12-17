open Components
open Tsdl

exception FaultyImage

type t = CornECS.entity

let create_player fl ren =
  CornECS.next_id () |> Sprite.b |> AnimatedSprite.b |> KeyboardController.b
  |> Renderable.s (Textman.load_texture fl ren, ren)

(**[paste curr size lim acc] is the list of squares (represented by [Sdl.rect]s)
   of size [size] that are contained in an image that is [lim] pixels wide.
   Raises: [FaultyImage] if [size] is not a divisor of [lim].*)
let rec paste curr size lim acc =
  match curr with
  | _ when curr = lim -> acc
  | _ when curr < lim ->
      paste (curr + size) size lim (Sdl.Rect.create curr 0 size size :: acc)
  | _ -> raise FaultyImage

(**[make_anim tex] is the sequence of squares (represented by [Sdl.rect]s) of
   size [Constants.spritesize] contained in the texture [tex].*)
let make_anim tex =
  let _, _, (w, h) = Sdl.query_texture tex |> Result.get_ok in
  paste 0 Constants.spritesize w [] |> Array.of_list

(**[add_walk_anim e dir sprst] associates the direction [dir] with the
   spritesheet [sprst] for the [WalkAnim]s of entity [e]. Requires: [sprst] is a
   valid [Sdl.texture] that has a width divisible by [Constants.spritesize].*)
let add_walk_anim e dir sprst =
  Hashtbl.add (WalkAnim.get e) dir (sprst, make_anim sprst)

(**[add_idle_tex e dir sprst fr] associates the direction [dir] with frame [fr]
   on the spritesheet [sprst] for the [IdleAnim]s of entity [e].

   Requires: [sprst] is a valid [Sdl.texture] that has a width divisible by
   [Constants.spritesize]. [fr] is a valid frame on [sprst].*)

let add_idle_anim e dir sprst fr =
  Hashtbl.add (IdleAnim.get e) dir (sprst, (make_anim sprst).(fr))

let init_anims player ~n ~e ~s ~w =
  add_walk_anim player North n;
  add_walk_anim player East e;
  add_walk_anim player South s;
  add_walk_anim player West w;
  add_idle_anim player North n 1;
  add_idle_anim player East e 1;
  add_idle_anim player South s 1;
  add_idle_anim player West w 1

[@@@warning "-8"]

(**[get_anim e dir fr] is the texture and rectangle to be displayed if [e] was
   facing [dir] and on frame [fr].

   Requires: [fr] is within bounds for the animation associated with [dir].*)
let get_anim e dir fr =
  ( Hashtbl.find (WalkAnim.get e) dir |> fst,
    Hashtbl.find (WalkAnim.get e) dir |> snd |> fun a -> Array.get a fr )

let draw e cam =
  let curr_frame = Frames.get e in
  let anim_frame = curr_frame / Constants.anim_speed in
  let _, ren = Renderable.get e in
  let _, dst = Rectangle.get e in
  let dir = Direction.get e in
  let tex, src =
    get_anim e dir anim_frame
    (* else Hashtbl.find (IdleAnim.get e) dir *)
  in
  Rectangle.set e (src, dst);
  Renderable.set e (tex, ren);
  if Moving.get e then Frames.set e (curr_frame + 1);
  if Frames.get e / Constants.anim_speed > 1 then Frames.set e 0;
  if dir = East then
    Textman.draw_flipped ~offset:(Camera.get_pos cam) tex ren src dst
    (*if the player is facing [East], then horizontally flip its side texture
      when rendering it.*)
  else Textman.draw ~offset:(Camera.get_pos cam) tex ren src dst

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
  if not (Moving.get e) then
    if (Keyboard.get e).{Sdl.Scancode.w} = 1 then (
      Moving.set e true;
      Direction.set e North;
      TargetPosition.set e (fst target, snd target - Constants.tilesize))
    else if (Keyboard.get e).{Sdl.Scancode.a} = 1 then (
      Moving.set e true;
      Direction.set e West;
      TargetPosition.set e (fst target - Constants.tilesize, snd target))
    else if (Keyboard.get e).{Sdl.Scancode.s} = 1 then (
      Moving.set e true;
      Direction.set e South;
      TargetPosition.set e (fst target, snd target + Constants.tilesize))
    else if (Keyboard.get e).{Sdl.Scancode.d} = 1 then (
      Moving.set e true;
      Direction.set e East;
      TargetPosition.set e (fst target + Constants.tilesize, snd target))
    else Moving.set e false

(**[check e m (x, y)] checks if [e] will collide with a collidable tile on [m]
   at the pixel coordinates [(x,y)]. Requires: [x] and [y] must both be divisors
   of [Constants.tilesize].*)
let check e m (x, y) =
  List.mem
    (Tilemap.get_tile m (x / Constants.tilesize, y / Constants.tilesize))
    (Tilemap.get_tileset m |> Tileset.get_colliders)

(**[check_collision e m] is whether [e] will collide with a collidable tile on
   [m] if it attempts to move to its [TargetPosition].*)
let check_collision e m =
  let tarx, tary = TargetPosition.get e in
  try check e m (tarx, tary) with Invalid_argument _ -> true

let update e m =
  let xtar, ytar = TargetPosition.get e in
  let xpos, ypos = Position.get e in
  (* if TargetPosition.get e = Position.get e then (
    Moving.set e false;
    update_vel e 0 0); *)
  if not (check_collision e m) then (
    if xtar > xpos then update_vel e Constants.player_speed 0;
    if xtar < xpos then update_vel e (-Constants.player_speed) 0;
    if ytar > ypos then update_vel e 0 Constants.player_speed;
    if ytar < ypos then update_vel e 0 (-Constants.player_speed))
  else TargetPosition.set e (xpos, ypos);
  update_pos e

(**[set_spawn player m] sets the [player]'s spawn point on map [m].*)
let set_spawn player m =
  let spawn_x = Tilemap.get_spawn m |> fst in
  let spawn_y = (Tilemap.get_spawn m |> snd) - Constants.tilesize in
  let scaled_spawn_x, scaled_spawn_y =
    (spawn_x * Tilemap.scale m, spawn_y * Tilemap.scale m)
  in
  Rectangle.set player
    ( fst (Rectangle.get player),
      Sdl.Rect.create scaled_spawn_x scaled_spawn_y Constants.tilesize
        Constants.tilesize );
  Position.set player (scaled_spawn_x, scaled_spawn_y);
  TargetPosition.set player (scaled_spawn_x, scaled_spawn_y)
