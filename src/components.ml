open Tsdl

module CornECS = ECS.Make (ECS.IntEnt)
(**Module representing the ECS system used in-game.*)

(**Module representing the "position" property.*)
module Position = struct
  type pos = int * int
  (**Type representing position, in the form of cartesian coordinates.*)

  include (val CornECS.new_property () : CornECS.PROPERTY with type t = pos)
end

module Event = struct
  type ev = Sdl.event

  include (val CornECS.new_property ~default:(Sdl.Event.create ()) ()
             : CornECS.PROPERTY
             with type t = ev)
end

(**Module representing the "move time" property, possessed by components that
   move on the map grid, such as [KeyboardController] or [AnimatedSprite].*)
module MoveTime = struct
  type time = int

  include (val CornECS.new_property ~default:0 () : CornECS.PROPERTY
             with type t = time)
end

(**Module representing the "velocity" property.*)
module Velocity = struct
  type vel = int * int
  (**Type representing velocity, in the form of an integer tuple.*)

  include (val CornECS.new_property ~default:(0, 0) () : CornECS.PROPERTY
             with type t = vel)
end

(**Module representing the "rectangle" property.*)
module Rectangle = struct
  type rects = Sdl.rect * Sdl.rect
  (**Type containing a tuple of [Sdl.rect]s, meant to represent a [src]
     rectangle and a [dst] rectangle for rendering purposes.*)

  include (val CornECS.new_property
                 ~default:
                   ( Sdl.Rect.create 0 0 Constants.spritesize
                       Constants.spritesize,
                     Sdl.Rect.create 0 0 Constants.tilesize Constants.tilesize
                   )
                 () : CornECS.PROPERTY
             with type t = rects)
end

(**Module representing the "direction" property.*)
module Direction = struct
  type dir =
    | North
    | South
    | West
    | East  (**Type representing direction.*)

  include (val CornECS.new_property ~default:North () : CornECS.PROPERTY
             with type t = dir)
end

(**Module representing the "renderable" property.*)
module Renderable = struct
  type resources = Sdl.texture * Sdl.renderer
  (**Type containing resources required to render a [Renderable] entity,
     including a [Sdl.texture] and a [Sdl.renderer].*)

  include (val CornECS.new_property () : CornECS.PROPERTY
             with type t = resources)
end

module Transform = struct
  include
    (val CornECS.new_component [ (module Position); (module Velocity) ] [])

  let update e =
    let posx, posy = Position.get e in
    let velx, vely = Velocity.get e in
    Position.set e (posx + velx, posy + vely)
end

module Collider = struct
  include
    (val CornECS.new_component [ (module Rectangle) ] [ (module Transform) ])

  (**[check e] checks if [e]'s next movement is valid, i.e. is [e] running into
     a wall.*)
  let check e = failwith "unimp"
end

(**A [Sprite] is a [Rectangle] that is [Renderable], has a [Velocity], and can
   update its own [Position]. It also has a [Collider] component.

   When creating an entity with the [Sprite] component, be sure to provide it a
   valid [Sdl.texture] and [Sdl.renderer] via [Textman.load_texture].*)
module Sprite = struct
  include (val CornECS.new_component
                 [
                   (module Position);
                   (module Rectangle);
                   (module Velocity);
                   (module Renderable);
                   (module Direction);
                 ]
                 [ (module Collider) ] : CornECS.COMPONENT)

  let string_of_tuple (a, b) = string_of_int a ^ ", " ^ string_of_int b

  (**[update e x y] updates the [Velocity] of entity [e], incrementing its
     x-directional speed by [x] and its y-directional speed by [y].*)
  let update e x y =
    let xpos, ypos = Position.get e in
    let velx, vely = Velocity.get e in
    let dst = snd (Rectangle.get e) in
    Velocity.set e (velx + x, vely + y);
    Sdl.Rect.set_x dst xpos;
    Sdl.Rect.set_y dst ypos;
    print_endline ("velocity: " ^ string_of_tuple (Velocity.get e))

  (**[draw e] draws the entity [e] onscreen.*)
  let draw e =
    let tex, ren = Renderable.get e in
    let src, dst = Rectangle.get e in
    ignore (Sdl.render_copy ~src ~dst ren tex)
end

module KeyboardController = struct
  include
    (val CornECS.new_component
           [ (module Event); (module MoveTime) ]
           [ (module Sprite); (module Transform) ])

  let facing e d =
    match d with
    | 0 -> Direction.get e = North
    | 1 -> Direction.get e = South
    | 2 -> Direction.get e = West
    | 3 -> Direction.get e = East
    | _ -> failwith "invalid direction"

  let move e d =
    let frames_to_cross_tile = Constants.tilesize / Constants.player_speed in
    (if MoveTime.get e <= 0 then
     match d with
     | 0 -> Direction.set e North
     | 1 -> Direction.set e South
     | 2 -> Direction.set e West
     | 3 -> Direction.set e East
     | _ -> failwith "invalid direction");
    MoveTime.set e frames_to_cross_tile;
    if MoveTime.get e <= 1 && facing e d then
      MoveTime.set e (MoveTime.get e + frames_to_cross_tile)

  (**[handle ev e] handles the Sdl event [ev] for the keyboard-controlled entity
     [e].*)
  let handle ev e = failwith "unimp"
  (* let player_speed = 4 in let frames_for_one_tile = Constants.tilesize /
     player_speed in let move_time = MoveTime.get e in match Sdl.Event.(enum
     (get ev typ)) with | `Key_down when Sdl.Event.get ev
     Sdl.Event.keyboard_repeat = 0 && match Action.get e with | Stop _ -> true |
     _ -> false -> ( match Sdl.Event.get ev Sdl.Event.keyboard_keycode |>
     Sdl.get_key_name with | "W" -> Action.set e (Move Up); Sprite.update e 0
     player_speed | "A" -> Action.set e (Move Left); Sprite.update e 0
     (-player_speed) | "S" -> Action.set e (Move Down); Sprite.update e
     player_speed 0 | "D" -> Action.set e (Move Right); Sprite.update e
     (-player_speed) 0 | _ -> ()) | `Key_up when Sdl.Event.get ev
     Sdl.Event.keyboard_repeat = 0 -> ( match Sdl.Event.get ev
     Sdl.Event.keyboard_keycode |> Sdl.get_key_name with | "W" when Action.get e
     = Move Up -> Action.set e (Stop Up) | "A" when Action.get e = Move Left ->
     Action.set e (Stop Left) | "S" when Action.get e = Move Down -> Action.set
     e (Stop Down) | "D" when Action.get e = Move Right -> Action.set e (Stop
     Right) | _ -> ()) | _ -> () *)

  (**[update e] updates the keyboard-controlled entity [e] based on what action
     [e] took after validating that said action is valid.*)
  let update e =
    let move_time = MoveTime.get e in
    if move_time > 0 then (
      MoveTime.set e (move_time - 1);
      Transform.update e)
end