(** Representation of dynamic adventure state.

    This module represents the state of an adventure as it is being played,
    including the adventurer's current room, the rooms that have been visited,
    and functions that cause the state to change. *)

type t
(** The abstract type of values representing the game state. *)

val init_state : Adventure.t -> t
(** [init_state a] is the initial state of the game when playing adventure [a].
    In that state the adventurer is currently located in the starting room, and
    they have visited only that room. *)

val current_room_id : t -> string
(** [current_room_id st] is the identifier of the room in which the adventurer
    is currently located in state [st]. *)

val visited : t -> string list
(** [visited st] is a set-like list of the room identifiers the adventurer has
    visited in state [st]. The adventurer has visited a room [rm] if their
    current room location is or has ever been [rm]. *)

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of t
  | Illegal

val go : Command.direction -> Adventure.t -> t -> result
(** [go dir adv st] is the result of attempting to go in the direction [dir] in
    state [st] and adventure [adv]:

    - If [dir] is the name of a cardinal direction in which another enterable
      room exists, then the result is [Legal st'], where in [st'] the adventurer
      is now located in the room to which travelling in [dir] leads.

    - Otherwise, the result is [Illegal].

    Effects: none. In particular, [go] does not print anything. *)

val fight : Command.action -> Adventure.t -> Character.character -> result
(** [fight act st ch] is the result of attempting to take the action [act] in
    state [st] and adventure [adv] while controlling character [ch]:

    - If the player is currently in a battle AND [act] is a valid action for
      [ch] to take, then the result is [Legal st'], where in [st'] the result of
      the player's actions are reflected.

    - Otherwise, the result is [Illegal].

    Effects: none. In particular, [fight] does not print anything. *)