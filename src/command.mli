(** Parsing of player commands. *)

(** The type [direction] represents the direction that can be part of a player's
    go command. Only four directions can be travelled in: North, South, East,
    and West. *)
type direction =
  | North
  | South
  | East
  | West

(** The type [action] represents the action that can be part of a player's fight
    command. The player can choose to physically attack, guard, cast a spell, or
    attempt to flee. The [Attack] action carries a [string] that represents the
    target's name; the [Cast] action carries a [string] tuple that represents
    the target's name and the spell's name, respectively.*)
type action =
  | Attack of string
  | Guard
  | Cast of (string * string)
  | Flee

(** The type [command] represents a player command that is decomposed into a
    verb and possibly some extra parameters, including a direction, or maybe an
    action. Invariant: the [direction] carried by [Go] must not be empty, and
    the [action] carried by [Fight] must not be empty. *)
type command =
  | Go of direction
  | Fight of action
  | Quit

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes
    the verb. The rest of the words, if any, become the parameters. Examples:

    - [parse "    go   north   "] is [Go North]
    - [parse "  fight   cast   clarkson    fireball"] is
      [Fight Cast ("clarkson", "fireball")]
    - [parse "quit"] is [Quit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command is malformed if
    the verb is not "quit", "go", or "fight", if the verb is "quit" and any
    other word after it, if the verb is "go" and the input for [direction] is
    not of the the four cardinal directions, or if the verb is "fight" and there
    is an empty or incomplete input for either the [Attack] or [Cast] commands.*)
