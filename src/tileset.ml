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

let load_tileset fl ren =
  let file = Yojson.Basic.from_file fl in
  let source = file |> member "image" |> to_string in
  let columns = file |> member "columns" |> to_int in
  {
    name = file |> member "name" |> to_string;
    source;
    firstgid = 1;
    lastgid = file |> member "tiles" |> index (-1) |> member "id" |> to_int;
    tex = Textman.load_texture source ren;
    rows = (file |> member "tilecount" |> to_int) / columns;
    columns;
    tilesize = file |> member "tileheight" |> to_int;
    collidables =
      file |> member "tiles" |> to_list |> filter_member "id" |> filter_int;
  }

let get_tile tset gid =
  Tsdl.Sdl.Rect.create
    ((gid - tset.firstgid) mod tset.columns * tset.tilesize)
    ((gid - tset.firstgid) / tset.columns * tset.tilesize)
    tset.tilesize tset.tilesize

let get_tex tset = tset.tex