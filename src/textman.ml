open Tsdl_image
open Tsdl

let load_texture file_name ren =
  let tmpSurface = Image.load file_name |> Result.get_ok in
  let tex = Sdl.create_texture_from_surface ren tmpSurface |> Result.get_ok in
  Sdl.free_surface tmpSurface;
  tex

let draw ?(offset = (0, 0)) tex ren src dst =
  let true_dst =
    Sdl.Rect.create
      (Sdl.Rect.x dst - fst offset)
      (Sdl.Rect.y dst - snd offset)
      (Sdl.Rect.w dst) (Sdl.Rect.h dst)
  in
  Sdl.render_copy ~src ~dst:true_dst ren tex |> Result.get_ok

let draw_flipped ?(offset = (0, 0)) tex ren src dst =
  let true_dst =
    Sdl.Rect.create
      (Sdl.Rect.x dst - fst offset)
      (Sdl.Rect.y dst - snd offset)
      (Sdl.Rect.w dst) (Sdl.Rect.h dst)
  in

  Sdl.render_copy_ex ~src ~dst:true_dst ren tex 0. None Sdl.Flip.horizontal
  |> Result.get_ok
