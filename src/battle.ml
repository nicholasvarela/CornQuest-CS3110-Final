type party_state = { party : Character.character list }
(*TODO: implement*)

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

let attack (enem : Character.character) (actor : Character.character) =
  let avoid = Character.get_attribute (Speed (-1.)) enem in
  let player_hit_chance =
    ((Character.get_attribute (Accuracy (-1.)) actor +. 60.) /. 100.0)
    -. (0.1 *. (avoid *. avoid))
  in
  let rand = Random.float 1. in
  if rand <= player_hit_chance then
    let damage =
      (Character.get_attribute (Defense (-1.)) enem /. 2.)
      -. Character.get_attribute (Strength (-1.)) actor
    in
    Character.adjust damage enem "hp"
  else enem

let enem_guard enem = Character.adjust_temps (Defense 10.) enem

let guard (actor : Character.character) =
  Character.adjust_temps (Defense 10.) actor
(*TODO: figure this out. thanks Aadarsh*)

let enemy_move_helper (enem : Character.character) (actor : Character.character)
    (lst : float list) (rand : float) =
  match lst with
  | h :: t -> (
      if rand < h then (actor, attack actor enem)
      else
        match t with
        | h :: t -> (
            if rand < h then (actor, enem_guard enem)
            else
              match t with
              | h :: t ->
                  Character.use_skill
                    (List.hd (Character.get_skills enem))
                    enem actor))

let pick_enemy_move (enem : Character.character) (actor : Character.character) =
  enemy_move_helper enem actor
    (Character.get_enem_move_chance enem)
    (Random.float 1.)
