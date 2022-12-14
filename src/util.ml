(**Various utility functions used throughout the gamecode.*)

(**[unwrap sdl_elt] is [sdl_elt] if the process creating it completed without
   errors. Otherwise, it raises an exception carrying the message stored in
   [sdl_elt].*)
let unwrap sdl_elt =
  match sdl_elt with
  | Ok elt -> elt
  | Error (`Msg err) -> failwith err

(**[pp_int_tuple (a,b)] is the string representation of the integer tuple
   [(a,b)].*)
let pp_int_tuple (a, b) = string_of_int a ^ ", " ^ string_of_int b

(**[(a, b) / div] = [(a / div, b / div)], where [( / )] is [Stdlib.(/)].*)
let ( / ) (a, b) div = (a / div, b / div)

(**[(a, b) - (c, d)] = [(a - c, b - d)], where [( - )] is [Stdlib.(-)].*)
let ( - ) (a, b) (c, d) = (a - c, b - d)

(**[(a, b) + (c, d)] = [(a + c, b + d)], where [( + )] is [Stdlib.(+)].*)
let ( + ) (a, b) (c, d) = (a + c, b + d)
