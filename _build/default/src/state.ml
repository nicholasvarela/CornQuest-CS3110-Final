(* type t = {
  current_room : string;
  visited_rooms : string list;
  in_battle : bool;
  (* #TODO *)
  party : int list;
}

let init_state adv =
  let start = Adventure.start_room adv in
  {
    current_room = start;
    visited_rooms = [ start ];
    in_battle = false;
    party = [];
  }

let current_room_id st = st.current_room
let visited st = st.visited_rooms

type result =
  | Legal of t
  | Illegal

let rec try_dir ex adv st acc = function
  | x :: xs ->
      if x = ex then
        let new_current_room = Adventure.next_room adv st.current_room x in
        Legal
          {
            current_room = new_current_room;
            visited_rooms =
              List.sort_uniq String.compare (new_current_room :: acc);
            in_battle = false;
            party = [];
          }
      else try_dir ex adv st acc xs
  | [] -> Illegal

(* let go ex adv st = try_dir ex adv st st.visited_rooms (Adventure.dirs adv
   st.current_room) *) *)
