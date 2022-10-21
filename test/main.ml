open OUnit2
open Game.Adventure

let prefix = "data" ^ Filename.dir_sep
let rhodes = Yojson.Basic.from_file (prefix ^ "demo_game.json")
let rhodes = from_json rhodes
let start_test (name : string) : test = name >:: fun _ -> assert_equal true true

let initial_room_test (name : string) (a : t) (expected_output : string) : test
    =
  name >:: fun _ -> assert_equal expected_output (start_room a)

let basic_tests = [ start_test "n" ]

let adventure_tests =
  [ initial_room_test "Inital Room of Rhodes is [lobby]" rhodes "lobby" ]

let suite =
  "test suite for Final Project"
  >::: List.flatten [ basic_tests; adventure_tests ]

let _ = run_test_tt_main suite
