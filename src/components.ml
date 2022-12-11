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

(**Module representing the "rectangle" property.*)
module Rectangle = struct
  type rects = Sdl.rect * Sdl.rect
  (**Type containing a tuple of [Sdl.rect]s, meant to represent a [src]
     rectangle and a [dst] rectangle for rendering purposes.*)

  include (val CornECS.new_property
                 ~default:(Sdl.Rect.create 0 0 0 0, Sdl.Rect.create 0 0 0 0)
                 () : CornECS.PROPERTY
             with type t = rects)
end

(**Module representing the "renderable" property.*)
module Renderable = struct
  type resources = Sdl.texture * Sdl.renderer
  (**Type containing resources required to render a [Renderable] entity,
     including a [Sdl.texture] and a [Sdl.renderer].*)

  include (val CornECS.new_property () : CornECS.PROPERTY
             with type t = resources)
end

(**A [Sprite] is a [Rectangle] that is [Renderable] and can update its own
   [Position].

   When creating an entity with the [Sprite] component, be sure to provide it a
   valid [Sdl.texture] and [Sdl.renderer] via [Textman.load_texture].*)
module Sprite = struct
  include (val CornECS.new_component
                 [ (module Position); (module Rectangle); (module Renderable) ]
                 [] : CornECS.COMPONENT)

  (**[update e x y] updates the [Position] of entity [e], incrementing its
     x-coordinate by [x] and y-coordinate by [y].*)
  let update e x y =
    let _, _, (w, h) =
      Sdl.query_texture (fst (Renderable.get e)) |> Util.unwrap
    in

    let xpos = fst (Position.get e) in
    let ypos = snd (Position.get e) in
    let src = fst (Rectangle.get e) in
    let dst = snd (Rectangle.get e) in
    Position.set e (xpos + x, ypos + y);
    Sdl.Rect.set_w src w;
    Sdl.Rect.set_h src h;
    Sdl.Rect.set_w dst w;
    Sdl.Rect.set_h dst h;
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
