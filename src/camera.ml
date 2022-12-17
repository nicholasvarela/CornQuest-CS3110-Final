open Components
open Util

type t = CornECS.entity

let create_camera e =
  CornECS.next_id () |> Cam.b
  |> Position.s
       (Position.get e
       + ((Constants.tilesize, Constants.tilesize) / 2)
       - ((Constants.screen_width, Constants.screen_height) / 2))

let track cam e =
  Position.set cam
    (Position.get e
    + ((Constants.tilesize, Constants.tilesize) / 2)
    - ((Constants.screen_width, Constants.screen_height) / 2))

let delete cam = CornECS.delete cam
let get_pos cam = Position.get cam