type dir =
  | North
  | South
  | West
  | East
      (**Variant type representing direction, for use in the [Direction]
         property.*)

module CornECS : ECS.S
(**Module representing the ECS system used in-game.*)

module Position : CornECS.PROPERTY with type t = int * int
(**Module representing the "position" property.*)

module Velocity : CornECS.PROPERTY with type t = int * int
(**Module representing the "velocity" property.*)

module Keyboard :
  CornECS.PROPERTY
    with type t = (int, Bigarray.int8_unsigned_elt) Tsdl.Sdl.bigarray
(**Module representing the "keyboard" property, possessed by components that
   listen for changes in keyboard state.*)

module Moving : CornECS.PROPERTY with type t = bool
(**Module representing the "moving" property, possessed by components that have
   the potential to move.*)

module TargetPosition : CornECS.PROPERTY with type t = int * int
(**Module representing the "target coordinate" property, possessed by components
   on the map that are bound to movement between tiles, such as
   [KeyboardController] or [AnimatedSprite].*)

module Rectangle : CornECS.PROPERTY with type t = Tsdl.Sdl.rect * Tsdl.Sdl.rect
(**Module representing the "rectangle" property, possessed by components that
   are rectangularly represented in-game (even if they aren't actual
   rectangles).*)

module Direction : CornECS.PROPERTY with type t = dir
(**Module representing the "direction" property, possessed by components that
   face one of four cardinal directions in-game.*)

module Renderable :
  CornECS.PROPERTY with type t = Tsdl.Sdl.texture * Tsdl.Sdl.renderer
(**Module representing the "renderable" property, possessed by components that
   are renderable onscreen/visible.*)

module IdleAnim :
  CornECS.PROPERTY
    with type t = (dir, Tsdl.Sdl.texture * Tsdl.Sdl.rect) Hashtbl.t
(**Module representing the "idle animation" property, possessed by components
   that switch textures based on the direction they face.*)

module WalkAnim :
  CornECS.PROPERTY
    with type t = (dir, Tsdl.Sdl.texture * Tsdl.Sdl.rect array) Hashtbl.t
(**Module representing the "walking animation" property, possessed by components
   that are animated based on the direction they are moving in.*)

module Frames : CornECS.PROPERTY with type t = int
(**Module representing the "frames" property, possessed by components that have
   frame-dependent behavior (such as animated sprites).*)

module Sprite : CornECS.COMPONENT
(**A [Sprite] is a [Rectangle] that is [Renderable], has [Velocity] and
   [Position] properties, is capable of [Moving], and is facing a [Direction].

   When creating an entity with the [Sprite] component, be sure to provide it a
   valid [Sdl.texture] and [Sdl.renderer] via [Textman.load_texture].*)

module KeyboardController : CornECS.COMPONENT
(**A [KeyboardController] is a component attached to [Sprite]s that are
   controlled by the keyboard. It requires listening to the [Keyboard].*)

module AnimatedSprite : CornECS.COMPONENT
(**An [AnimatedSprite] is a [Sprite] that is also animated.

   When creating an entity with the [AnimatedSprite] component, be sure to
   provide it a valid [Sdl.renderer], as well as valid [Sdl.texture]s to switch
   between.*)

module Cam : CornECS.COMPONENT
(**A [Cam] component.*)
