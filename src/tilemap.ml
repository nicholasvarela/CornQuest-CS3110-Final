open Tsdl
open Yojson.Basic.Util
open Bigarray

type t = {
  tilesize : int;
  height : int;
  width : int;
  dst : Sdl.rect;
  tileset : Tileset.t;
  tiles : (int, int_elt, c_layout) Array2.t;
  spawn : int * int;
}

(**[chunk cols arr] takes a one-dimensional int array [arr] and divides it into
   a two-dimensional int array, composed of [(Array.length arr / cols)] arrays
   with that each contain [cols] elements.*)
let chunk cols arr =
  let start = ref 0 in
  let len = Array.length arr in
  assert (len mod cols = 0);
  let rows = len / cols in
  let acc = Array.make_matrix rows cols 0 in
  let ct = ref 0 in
  while !start < len do
    acc.(!ct) <- Array.sub arr !start cols;
    start := !start + cols;
    incr ct
  done;
  acc

let load_map fl ren =
  let file = Yojson.Basic.from_file fl in
  let tset = file |> member "tilesets" |> index 0 in
  let tilefile = tset |> member "source" |> to_string in
  let fgid = tset |> member "firstgid" |> to_int in
  let width = file |> member "width" |> to_int in
  let tiledata =
    file |> member "layers" |> index 0 |> member "data" |> to_list
    |> List.map to_int |> Array.of_list |> chunk width
  in
  let tilesize = file |> member "tileheight" |> to_int in
  let spawn_obj =
    file |> member "layers" |> to_list
    |> List.find (fun layer -> layer |> member "name" |> to_string = "Player")
    |> member "objects" |> to_list
    |> List.find (fun obj -> obj |> member "name" |> to_string = "Spawn")
  in
  let spawn =
    (spawn_obj |> member "x" |> to_int, spawn_obj |> member "y" |> to_int)
  in

  {
    tilesize;
    tileset =
      Tileset.load_tileset (Constants.data_dir_prefix ^ tilefile) ren fgid;
    dst = Sdl.Rect.create 0 0 Constants.tilesize Constants.tilesize;
    tiles = Array2.of_array Int c_layout tiledata;
    height = file |> member "height" |> to_int;
    width;
    spawn;
  }

let draw_map m ren cam =
  for i = 0 to m.height - 1 do
    for j = 0 to m.width - 1 do
      Sdl.Rect.set_x m.dst (j * Constants.tilesize);
      Sdl.Rect.set_y m.dst (i * Constants.tilesize);
      match Array2.get m.tiles i j with
      | ind ->
          Textman.draw ~offset:(Camera.get_pos cam)
            (Tileset.get_tex m.tileset)
            ren
            (Tileset.get_tile m.tileset ind)
            m.dst
    done
  done

let get_spawn m = m.spawn
let scale m = Constants.tilesize / m.tilesize
let get_tile m (x, y) = m.tiles.{y, x}
let get_tileset m = m.tileset
let get_dim m = (m.width * Constants.tilesize, m.height * Constants.tilesize)
