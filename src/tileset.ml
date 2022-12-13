open Yojson.Basic.Util

type t = {
  name : string;
  source : string;
  firstgid : int;
  lastgid : int;
  tex : Tsdl.Sdl.texture;
  rows : int;
  columns : int;
  tilesize : int;
  collidables : int list;
}

let load_tileset fl ren fgid =
  let file = Yojson.Basic.from_file fl in
  let source = file |> member "image" |> to_string in
  let columns = file |> member "columns" |> to_int in
  let tiles = file |> member "tiles" |> to_list in
  let collidables =
    tiles
    |> List.filter (fun tl ->
           tl |> member "properties" |> to_list
           |> List.find (fun pr ->
                  pr |> member "name" |> to_string = "collides")
           |> member "value" |> to_bool)
    |> filter_member "id" |> filter_int
    |> List.map (fun i -> i + fgid)
  in
  {
    name = file |> member "name" |> to_string;
    source;
    firstgid = fgid;
    lastgid = file |> member "tiles" |> index (-1) |> member "id" |> to_int;
    tex = Textman.load_texture (Constants.data_dir_prefix ^ source) ren;
    rows = (file |> member "tilecount" |> to_int) / columns;
    columns;
    tilesize = file |> member "tileheight" |> to_int;
    collidables;
  }

let get_tile tset gid =
  Tsdl.Sdl.Rect.create
    ((gid - tset.firstgid) mod tset.columns * tset.tilesize)
    ((gid - tset.firstgid) / tset.columns * tset.tilesize)
    tset.tilesize tset.tilesize

let get_tex tset = tset.tex
let get_colliders tset = tset.collidables
