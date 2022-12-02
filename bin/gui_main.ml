open Tsdl

let unwrap = function
  | Ok thing -> thing
  | Error (`Msg err) -> failwith err

let main () =
  let _ = Sdl.init Sdl.Init.video in
  let window =
    Sdl.create_window "Title" ~x:Sdl.Window.pos_centered
      ~y:Sdl.Window.pos_centered ~w:800 ~h:600 Sdl.Window.shown
  in
  let renderer = Sdl.create_renderer ~index:(-1) (unwrap window) |> unwrap in
  Sdl.set_render_draw_color renderer 255 0 0 255 |> unwrap;
  Sdl.render_clear renderer |> unwrap;
  Sdl.render_present renderer;
  Sdl.delay 3000l
