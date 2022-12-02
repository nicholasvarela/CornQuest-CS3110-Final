open OUnit2
open Game
open Adventure
open Yojson

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether they are
    equivalent set-like lists. That means checking two things. First, they must
    both be "set-like", meaning that they do not contain any duplicates. Second,
    they must contain the same elements, though not necessarily in the same
    order. *)
(* let cmp_set_like_lists lst1 lst2 = let uniq1 = List.sort_uniq compare lst1 in
   let uniq2 = List.sort_uniq compare lst2 in List.length lst1 = List.length
   uniq1 && List.length lst2 = List.length uniq2 && uniq1 = uniq2 *)

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(***asdfasdfs*)

let prefix = "data" ^ Filename.dir_sep
let rhodes = Yojson.Basic.from_file (prefix ^ "demo_game.json")
let rhodes = from_json rhodes
let start_test (name : string) : test = name >:: fun _ -> assert_equal true true

let dirs_test (name : string) (a : Adventure.t) (s : string)
    (expected_output : string list) : test =
  name >:: fun _ ->
  assert_equal expected_output (dirs a s) ~printer:(pp_list pp_string)

let initial_room_test (name : string) (a : Adventure.t)
    (expected_output : Adventure.obj) : test =
  name >:: fun _ -> assert_equal expected_output (spawn a)

(** constrcuts a OUnit test named [name ] that asserts the quality of
    [expected_output] with [description] *)
let description_test (name : string) (a : Adventure.t) (s : string)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (description a s)

let basic_tests = [ start_test "n" ]
let adventure_tests = []

let suite =
  "test suite for Final Project"
  >::: List.flatten [ basic_tests; adventure_tests ]

let _ = run_test_tt_main suite