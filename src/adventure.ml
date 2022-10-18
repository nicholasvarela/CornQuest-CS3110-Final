open Yojson.Basic.Util

exception UnknownRoom of string
exception UnknownExit of string

type exit = {
  name : string;
  room_id : string;
}
(** The type representing an exit from a room in an adventure. *)

type mob = {
  name : string;
  hp : int;
  mp : int;
  exp_given : int;
  lvl : int;
  skills : string list;
}
(** The type representing a monster that can be found in an adventure.*)

type room = {
  id : string;
  description : string;
  exits : exit list;
  mobs : mob list;
}

(** The type representing a room in an adventure. *)

type t = {
  rooms : room list;
  start_room : string;
  mob_chance : float;
}

(** [exit_of_json j] parses and extracts an exit from a room in [j]. *)
let exit_of_json json =
  {
    name = json |> member "name" |> to_string;
    room_id = json |> member "room id" |> to_string;
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

(** [room_of_json j] parses and extracts a room in [j]. *)
let room_of_json json =
  {
    id = json |> member "id" |> to_string;
    description = json |> member "description" |> to_string;
    exits = json |> member "exits" |> to_list |> List.map exit_of_json;
    mobs = json |> member "mobs" |> to_list |> List.map mob_of_json;
  }

let from_json json =
  {
    rooms = json |> member "rooms" |> to_list |> List.map room_of_json;
    start_room = json |> member "start room" |> to_string;
    mob_chance = json |> member "mob chance" |> to_float;
  }

let start_room adv = adv.start_room
let room_ids adv = List.map (fun room -> room.id) adv.rooms

let rec description adv room =
  match adv.rooms with
  | [] -> raise (UnknownRoom room)
  | { id; description = desc; _ } :: xs ->
      if id = room then desc else description { adv with rooms = xs } room

let rec dirs adv room =
  match adv.rooms with
  | [] -> raise (UnknownRoom room)
  | { id; exits = exs; _ } :: xs ->
      if id = room then List.map (fun (ex : exit) -> ex.name) exs
      else dirs { adv with rooms = xs } room

(** [find_exit dir exits] is the identifier of the room that is reached by
    exiting through the direction [dir], if it is a valid direction. Raises
    [UnknownExit dir] if there is no exit through [dir]. *)
let rec find_exit dir = function
  | [] -> raise (UnknownExit dir)
  | { name; room_id } :: xs -> if name = dir then room_id else find_exit dir xs

let rec next_room adv room ex =
  match adv.rooms with
  | [] -> raise (UnknownRoom room)
  | { id; exits; _ } :: xs ->
      if id = room then find_exit ex exits
      else next_room { adv with rooms = xs } room ex

let rec next_rooms adv room : string list =
  match adv.rooms with
  | [] -> raise (UnknownRoom room)
  | { id; exits; _ } :: xs ->
      if id = room then
        exits
        |> List.map (fun ex -> ex.room_id)
        |> List.sort_uniq String.compare
      else next_rooms { adv with rooms = xs } room
