open Tsdl_image
open Tsdl

let load_texture file_name ren =
  let tmpSurface = Image.load file_name |> Util.unwrap in
  let tex = Sdl.create_texture_from_surface ren tmpSurface |> Util.unwrap in
  Sdl.free_surface tmpSurface;
  tex

let draw tex ren src dst = Sdl.render_copy ~src ~dst ren tex |> Util.unwrap
