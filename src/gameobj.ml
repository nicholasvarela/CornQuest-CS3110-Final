open Tsdl_image
open Tsdl

type t = {
  mutable xpos : int;
  mutable ypos : int;
  ren : Sdl.renderer;
  tex : Sdl.texture;
  mutable src : Sdl.rect;
  mutable dst : Sdl.rect;
}

let create tx ren xpos ypos =
  let tex = Textman.load_texture tx ren in
  let _, _, (w, h) = Sdl.query_texture tex |> Util.unwrap in
  {
    xpos;
    ypos;
    ren;
    tex;
    src = Sdl.Rect.create 0 0 w h;
    dst = Sdl.Rect.create xpos ypos w h;
  }

let update obj x_inc y_inc =
  let _, _, (w, h) = Sdl.query_texture obj.tex |> Util.unwrap in

  Sdl.Rect.set_w obj.src w;
  Sdl.Rect.set_h obj.src h;
  obj.xpos <- obj.xpos + x_inc;
  obj.ypos <- obj.ypos + y_inc;
  Sdl.Rect.set_x obj.dst obj.xpos;
  Sdl.Rect.set_y obj.dst obj.ypos;
  Sdl.Rect.set_w obj.dst w;
  Sdl.Rect.set_h obj.dst h;
  ()

let render obj = Sdl.render_copy ~src:obj.src ~dst:obj.dst obj.ren obj.tex
