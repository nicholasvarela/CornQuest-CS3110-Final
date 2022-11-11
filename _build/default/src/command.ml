type direction =
  | North
  | South
  | East
  | West

type action =
  | Attack of string
  | Guard
  | Cast of (string * string)
  | Flee

type command =
  | Go of direction
  | Fight of action
  | Quit

exception Empty
exception Malformed

(** [parse_command com] parses a list of strings [com] into a [command], as
    follows. The first element of [com] becomes the verb. The remaining
    elements, if any, become the parameters.

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command is malformed if
    the first element is not ["quit"], ["go"], or ["fight"], if the verb is
    ["quit"] and any other word after it, if the verb is ["go"] and there is an
    the input for [direction] is not of the the four cardinal directions, or if
    the verb is "battle" and there is an empty or incomplete input for either
    the [Attack] or [Cast] commands.*)
let parse_command = function
  | str :: t ->
      if str = "go" then
        match t with
        | [ "north" ] -> Go North
        | [ "east" ] -> Go East
        | [ "south" ] -> Go South
        | [ "west" ] -> Go West
        | _ -> raise Malformed
      else if str = "quit" then if t <> [] then raise Malformed else Quit
      else if str = "battle" then
        match t with
        | [ "guard" ] -> Fight Guard
        | [ "flee" ] -> Fight Flee
        | [ "attack"; target ] -> Fight (Attack target)
        | [ "cast"; spell; target ] -> Fight (Cast (spell, target))
        | _ -> raise Malformed
      else raise Malformed
  | [] -> raise Empty

let parse str =
  str |> String.lowercase_ascii |> String.split_on_char ' '
  |> List.filter (fun s -> s <> "")
  |> parse_command