open Yojson.Basic.Util

exception UnknownRoom of string
exception UnknownExit of string

(** The type representing an object on a map. *)

type obj = {
  id : int;
  name : string;
  obj_class : string;
  height : int;
  width : int;
  x : int;
  y : int;
}

(** The type representing a monster that can be found in an adventure.*)

type mob = {
  name : string;
  hp : int;
  mp : int;
  exp_given : int;
  lvl : int;
  skills : string list;
}

(** The type representing the data in a map. *)

type t = {
  height : int;
  width : int;
  tiles : int list;
  spawn : obj;
  exits : obj list;
  chests : obj list;
  mob_chance : float;
  traversables : int list;
}
(** [mob_of_json j] parses and extracts an encounterable mob from a room in [j]. *)
let mob_of_json json =
  {
    name = json |> member "name" |> to_string;
    hp = json |> member "hp" |> to_int;
    mp = json |> member "mp" |> to_int;
    exp_given = json |> member "exp given" |> to_int;
    lvl = json |> member "level" |> to_int;
    skills = json |> member "skills" |> to_list |> List.map to_string;
  }

let obj_of_json json = {id = json |> member "id" |> to_int; name = json |> member "name" |> to_string; obj_class = json |> member "class" |> to_string; height = json |> member "height" |> to_int; width = json |> member "width" |> to_int; x = json |> member "x" |> to_int; y = json |> member "y" |> to_int;}

let from_json json = failwith "unimplemented"
  (* {
    height = json |> member "height" |> to_int;
    width = json |> member "width" |> to_int;
    tiles = json |> member "layers" |> member "data" |> List.map to_int |> to_list;
    spawn = json |> member "" |> to_string;
    exits = json |> member 
    mob_chance = json |> member "mob chance" |> to_float;
  } *)

let spawn m = m.spawn
(* let room_ids adv = List.map (fun room -> room.id) adv.rooms *)

let rec description adv room = failwith "unimplemented"
  (* match adv.rooms with
  | [] -> raise (UnknownRoom room)
  | { id; description = desc; _ } :: xs ->
      if id = room then desc else description { adv with rooms = xs } room *)

let rec dirs adv room = failwith "unimplemented"
  (* match adv.rooms with
  | [] -> raise (UnknownRoom room)
  | { id; exits = exs; _ } :: xs ->
      if id = room then List.map (fun (ex : exit) -> ex.name) exs
      else dirs { adv with rooms = xs } room *)

(** [find_exit dir exits] is the identifier of the room that is reached by
    exiting through the direction [dir], if it is a valid direction. Raises
    [UnknownExit dir] if there is no exit through [dir]. *)
let rec find_exit dir = failwith "unimplemented"
  (* function
  | [] -> raise (UnknownExit dir)
  | { name; room_id } :: xs -> if name = dir then room_id else find_exit dir xs *)

let rec next_room adv room ex =failwith "unimplemented"
  (* match adv.rooms with
  | [] -> raise (UnknownRoom room)
  | { id; exits; _ } :: xs ->
      if id = room then find_exit ex exits
      else next_room { adv with rooms = xs } room ex *)

let rec next_rooms adv room : string list =failwith "unimplemented"
  (* match adv.rooms with
  | [] -> raise (UnknownRoom room)
  | { id; exits; _ } :: xs ->
      if id = room then
        exits
        |> List.map (fun ex -> ex.room_id)
        |> List.sort_uniq String.compare
      else next_rooms { adv with rooms = xs } room *)
