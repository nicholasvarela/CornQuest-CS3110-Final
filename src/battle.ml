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

let wait un =
  let i = ref 0 in
  while !i < 100 do
    match read_line () with
    | _ -> i := 100
  done

let attack (enem : Character.character) (actor : Character.character) =
  Random.self_init ();
  let avoid = Character.get_attribute_val "speed" enem in
  let player_hit_chance =
    ((Character.get_attribute_val "accuracy" actor +. 60.) /. 100.0)
    -. (0.001 *. (avoid *. avoid))
  in
  let rand = Random.float 1. in
  if rand <= player_hit_chance then (
    let damage, extra_str =
      let raw = Character.get_total_attr_val "strength" actor in
      let norm =
        (Character.get_attribute_val "defense" enem /. 2.)
        +. (0.3 *. Character.get_temp_value "defense" enem)
        -. Character.get_total_attr_val "strength" actor
      in
      let extra, str =
        let r = Random.float 1. in
        if Character.get_total_attr_val "luck" actor /. 100. >= r then
          (raw, "It's a critical hit! ")
        else (0., "")
      in
      (norm -. extra, str)
    in
    let findmg = if damage > 0. then 0. else damage in
    let _ = wait () in
    print_string
      (extra_str ^ Character.get_name actor ^ " dealt "
      ^ string_of_float (if findmg = 0. then findmg else findmg *. -1.)
      ^ " damage!\n");
    let _ = wait () in

    Character.adjust findmg enem "hp")
  else
    let _ = wait () in
    let _ = print_string (Character.get_name actor ^ " missed!\n") in
    let _ = wait () in
    enem

let guard (actor : Character.character) =
  let a = Character.adjust_temps (Defense 10., 1) actor in
  let out = Character.adjust_temps (MagicResist 10., 1) a in
  let _ = if List.length actor.enem_hit_chances != 0 then wait () in
  out

let unwrap skop =
  match skop with
  | None -> failwith "None"
  | Some sk -> sk

let rec skill_traversal (actor, enem) lst rand index =
  match lst with
  | [] -> failwith "invalid enemy"
  | h :: t ->
      if rand < h then
        Character.use_skill
          (unwrap (Character.get_skills enem).(index))
          enem actor
      else skill_traversal (actor, enem) t rand (index + 1)

let rec enem_spell_helper (actor, enem) lst rand cnt acc =
  match lst with
  | [] -> failwith "invalid enemy spells"
  | h :: t ->
      let chance = h +. acc in
      if rand <= chance then (
        let sk = Character.unwrap_skill (Character.get_skills enem).(cnt) in
        ANSITerminal.print_string [ ANSITerminal.red ]
          ("\n" ^ Character.get_name enem ^ " used " ^ sk.name ^ "!");
        let a, b, _ = Character.use_skill sk enem actor in
        (a, b))
      else enem_spell_helper (actor, enem) t rand (cnt + 1) chance

let enemy_move_helper (actor, enem) (lst : float list) (rand : float) =
  match lst with
  | [] -> failwith "invalid enemy"
  | h :: t -> (
      let acc0 = h in
      if rand <= h then
        let _ =
          ANSITerminal.print_string [ ANSITerminal.red ]
            (Character.get_name enem ^ " attacked!")
        in
        (attack actor enem, enem)
      else
        match t with
        | [] -> failwith "invalid enemy"
        | h :: t ->
            if rand <= h +. acc0 then (
              ANSITerminal.print_string [ ANSITerminal.red ]
                (Character.get_name enem ^ " guarded!\n");
              (actor, guard enem))
            else enem_spell_helper (actor, enem) t rand 0 (h +. acc0))

let pick_enemy_move (actor, enem) =
  let a, e =
    enemy_move_helper (actor, enem)
      (Character.get_enem_move_chance enem)
      (Random.float 1.)
  in
  let a' = Character.clear_temps (ref a) in
  (a', e)
