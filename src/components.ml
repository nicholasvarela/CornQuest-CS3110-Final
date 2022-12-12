open Tsdl

module CornECS = ECS.Make (ECS.IntEnt)
(**Module representing the ECS system used in-game.*)

(**Module representing the "position" property.*)
module Position = struct
  type pos = int * int
  (**Type representing position, in the form of cartesian coordinates.*)

  include (val CornECS.new_property ~default:(0, 0) () : CornECS.PROPERTY
             with type t = pos)
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
                   ( Sdl.Rect.create 0 0 0 0,
                     Sdl.Rect.create 0 0 Constants.tilesize Constants.tilesize
                   )
                 () : CornECS.PROPERTY
             with type t = rects)
end

module Action = struct
  type direction =
    | Up
    | Down
    | Left
    | Right  (**Type representing an direction of movement.*)

  type act =
    | Idle
    | Move of direction
    | Bruh  (**Type representing an action.*)

  include (val CornECS.new_property ~default:Idle () : CornECS.PROPERTY
             with type t = act)
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
    let posx = fst (Position.get e) in
    let posy = snd (Position.get e) in
    let velx = fst (Velocity.get e) in
    let vely = snd (Velocity.get e) in
    Position.set e (posx + velx, posy + vely)
end

module Collider = struct
  include
    (val CornECS.new_component [ (module Rectangle) ] [ (module Transform) ])

  (**[check e] checks if [e]'s next movement is valid, i.e. is [e] running into
     a wall.*)
  let check e = failwith "unimp"
end

(**A [Sprite] is a [Rectangle] that is [Renderable] and can update its own
   [Position].

   When creating an entity with the [Sprite] component, be sure to provide it a
   valid [Sdl.texture] and [Sdl.renderer] via [Textman.load_texture].*)
module Sprite = struct
  include (val CornECS.new_component
                 [
                   (module Position);
                   (module Rectangle);
                   (module Velocity);
                   (module Renderable);
                 ]
                 [ (module Collider) ] : CornECS.COMPONENT)

  (**[update e x y] updates the [Velocity] of entity [e], making its
     x-directional speed [x] and its y-directional speed [y].*)
  let update e x y =
    let _, _, (w, h) =
      Sdl.query_texture (fst (Renderable.get e)) |> Util.unwrap
    in

    let xpos = fst (Position.get e) in
    let ypos = snd (Position.get e) in
    let src = fst (Rectangle.get e) in
    let dst = snd (Rectangle.get e) in
    Velocity.set e (x, y);
    Sdl.Rect.set_w src w;
    Sdl.Rect.set_h src h;
    Sdl.Rect.set_w dst Constants.tilesize;
    Sdl.Rect.set_h dst Constants.tilesize;
    Sdl.Rect.set_x dst xpos;
    Sdl.Rect.set_y dst ypos

  (**[draw e] draws the entity [e] onscreen.*)
  let draw e =
    let tex = fst (Renderable.get e) in
    let ren = snd (Renderable.get e) in
    let src = fst (Rectangle.get e) in
    let dst = snd (Rectangle.get e) in
    ignore (Sdl.render_copy ~src ~dst ren tex)
end

module KeyboardController = struct
  include
    (val CornECS.new_component
           [ (module Action) ]
           [ (module Sprite); (module Transform) ])

  let update e =
    (match Action.get e with
    | Idle -> Sprite.update e 0 0
    | Move dir -> (
        match dir with
        | Up -> Sprite.update e 0 (-10)
        | Down -> Sprite.update e 0 10
        | Left -> Sprite.update e (-10) 0
        | Right -> Sprite.update e 10 0)
    | Bruh -> ());
    Transform.update e
end
