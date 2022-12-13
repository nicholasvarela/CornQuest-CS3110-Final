(**Various utility functions used throughout the gamecode.*)

(**[unwrap sdl_elt] is [sdl_elt] if the process creating it completed without
   errors. Otherwise, it raises an exception carrying the message stored in
   [sdl_elt].*)
let unwrap sdl_elt =
  match sdl_elt with
  | Ok elt -> elt
  | Error (`Msg err) -> failwith err
