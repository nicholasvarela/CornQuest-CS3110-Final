(**Various utility functions used throughout the gamecode.*)

(**[pp_int_tuple (a,b)] is the string representation of the integer tuple
   [(a,b)].*)
let pp_int_tuple (a, b) = string_of_int a ^ ", " ^ string_of_int b

(**[(a, b) / div] = [(a / div, b / div)], where [( / )] is [Stdlib.(/)].*)
let ( / ) (a, b) div = (a / div, b / div)

(**[(a, b) - (c, d)] = [(a - c, b - d)], where [( - )] is [Stdlib.(-)].*)
let ( - ) (a, b) (c, d) = (a - c, b - d)

(**[(a, b) + (c, d)] = [(a + c, b + d)], where [( + )] is [Stdlib.(+)].*)
let ( + ) (a, b) (c, d) = (a + c, b + d)
