(*Formatters for in-game types*)

(**[dir_to_string dir] is the string representation of [dir], of type
   [Command.direction].*)
let dir_to_string (dir : Command.direction) =
  match dir with
  | North -> "north"
  | South -> "south"
  | East -> "east"
  | West -> "west"
