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

(*TODO: Extract all print statements and delegate it to bin/main.ml*)
let check_health (enem : Character.character) (actor : Character.character) =
  if Character.get_attribute "hp" enem < 0. then
    let _ = exit 0 in
    print_endline "Enemy Dead"
  else if Character.get_attribute "hp" actor < 0. then
    let _ = exit 0 in
    print_endline "Hero Dead"

let attack (enem : Character.character) (actor : Character.character) =
  check_health enem actor;
  let avoid = Character.get_attribute "speed" enem in
  let player_hit_chance =
    ((Character.get_attribute "accuracy" actor +. 60.) /. 100.0)
    -. (0.001 *. (avoid *. avoid))
  in
  let rand = Random.float 1. in
  if rand <= player_hit_chance then
    let damage =
      (Character.get_attribute "defense" enem /. 2.)
      +. Character.get_temps enem
      -. Character.get_attribute "defense" actor
    in
    let _ =
      print_endline
        (Character.get_name actor ^ " dealt "
        ^ string_of_float (damage *. -1.)
        ^ " damage!")
    in
    if damage < 0. then Character.adjust damage enem "hp" else enem
  else
    let _ = print_endline (Character.get_name actor ^ " missed!") in
    enem

let enem_guard enem = Character.adjust_temps (Defense 10., 2) enem

let guard (actor : Character.character) =
  Character.adjust_temps (Defense 2., 2) actor

let upwrap_skill g =
  match g with
  | Some x -> x
  | None -> failwith "up_wrap skill fail "

let enemy_move_helper (actor, enem) (lst : float list) (rand : float) =
  match lst with
  | [] -> failwith "invalid enemy"
  | h :: t -> (
      if rand <= h then
        let _ = print_endline (Character.get_name enem ^ " attacked!") in
        (attack actor enem, enem)
      else
        match t with
        | [] -> failwith "invalid enemy"
        | h :: t -> (
            if rand < h then (actor, enem_guard enem)
            else
              match t with
              | [] -> failwith "invalid enemy"
              | _ ->
                  Character.use_skill
                    (upwrap_skill (Character.get_skills enem).(0))
                    enem actor))

let pick_enemy_move (actor, enem) =
  enemy_move_helper (actor, enem)
    (Character.get_enem_move_chance enem)
    (Random.float 1.)
