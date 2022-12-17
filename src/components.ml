open Tsdl
module CornECS = ECS.Make (ECS.IntEnt)

type dir =
  | North
  | South
  | West
  | East

module Position = struct
  type pos = int * int
  (**Type representing position, in the form of cartesian coordinates.*)

  include (val CornECS.new_property () ~default:(0, 0) : CornECS.PROPERTY
             with type t = pos)
end

module IdleAnim = struct
  type animations = (dir, Sdl.texture * Sdl.rect) Hashtbl.t
  (**Type representing a series of idle animations, in the form of a hashtable
     mapping directions to tuples containing textures and [Sdl.rect]s, which
     represent idle animations.*)

  include (val CornECS.new_property ~default:(Hashtbl.create 4) ()
             : CornECS.PROPERTY
             with type t = animations)
end

module WalkAnim = struct
  type animations = (dir, Sdl.texture * Sdl.rect array) Hashtbl.t
  (**Type representing the animations an [Animated] component could have, in the
     form of a hashtable mapping directions to tuples containing textures as
     well as arrays of [Sdl.rect]s meant to represent animation frames.*)

  include (val CornECS.new_property ~default:(Hashtbl.create 4) ()
             : CornECS.PROPERTY
             with type t = animations)
end

module Frames = struct
  type frames = int
  (**Type meant to serve as a frame counter (for animated components).*)

  include (val CornECS.new_property ~default:0 () : CornECS.PROPERTY
             with type t = frames)
end

module Keyboard = struct
  type state = (int, Bigarray.int8_unsigned_elt) Tsdl.Sdl.bigarray

  include (val CornECS.new_property ~default:(Sdl.get_keyboard_state ()) ()
             : CornECS.PROPERTY
             with type t = state)
end

module Moving = struct
  type ev = bool
  (**Type representing whether something is capable of movement, in the form of
     a boolean value.*)

  include (val CornECS.new_property ~default:false () : CornECS.PROPERTY
             with type t = ev)
end

module TargetPosition = struct
  type tar = int * int
  (**Type representing target position, in the form of cartesian coordinates.*)

  include (val CornECS.new_property () ~default:(0, 0) : CornECS.PROPERTY
             with type t = tar)
end

module Velocity = struct
  type vel = int * int
  (**Type representing velocity, in the form of an integer tuple.*)

  include (val CornECS.new_property ~default:(0, 0) () : CornECS.PROPERTY
             with type t = vel)
end

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

module Direction = struct
  include (val CornECS.new_property ~default:North () : CornECS.PROPERTY
             with type t = dir)
end

module Renderable = struct
  type resources = Sdl.texture * Sdl.renderer
  (**Type containing resources required to render a [Renderable] entity,
     including a [Sdl.texture] and a [Sdl.renderer].*)

  include (val CornECS.new_property () : CornECS.PROPERTY
             with type t = resources)
end

module Sprite = struct
  include (val CornECS.new_component
                 [
                   (module Position);
                   (module Rectangle);
                   (module Velocity);
                   (module Renderable);
                   (module Direction);
                   (module Moving);
                   (module TargetPosition);
                 ]
                 [] : CornECS.COMPONENT)
end

module AnimatedSprite = struct
  include (val CornECS.new_component
                 [ (module WalkAnim); (module IdleAnim); (module Frames) ]
                 [ (module Sprite) ] : CornECS.COMPONENT)
end

module KeyboardController = struct
  include (val CornECS.new_component [ (module Keyboard) ] [])
end

module Cam = struct
  include (val CornECS.new_component [ (module Position) ] [])
end
