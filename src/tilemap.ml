open Tsdl
open Yojson.Basic.Util
open Bigarray

type t = {
  height : int;
  width : int;
  dst : Sdl.rect;
  tileset : Tileset.t;
  tiles : (int, int_elt, c_layout) Array2.t;
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

let load_map tx fl ren =
  let file = Yojson.Basic.from_file fl in
  let width = file |> member "width" |> to_int in
  let tiledata =
    file |> member "layers" |> index 0 |> member "data" |> to_list
    |> List.map to_int |> Array.of_list |> chunk width
  in
  {
    tileset = Tileset.load_tileset "data/cavetiles.json" ren;
    dst = Sdl.Rect.create 0 0 16 16;
    tiles = Array2.of_array Int c_layout tiledata;
    height = file |> member "height" |> to_int;
    width;
  }

let draw_map m ren =
  for i = 0 to Array2.dim1 m.tiles - 1 do
    for j = 0 to Array2.dim2 m.tiles - 1 do
      Sdl.Rect.set_x m.dst (j * 16);
      Sdl.Rect.set_y m.dst (i * 16);
      match Array2.get m.tiles i j with
      | i ->
          Textman.draw
            (Tileset.get_tex m.tileset)
            ren
            (Tileset.get_tile m.tileset i)
            m.dst
    done
  done
