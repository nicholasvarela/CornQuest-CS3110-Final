open OUnit2
open Game
open Adventure
open Yojson

(*Testing Plan: Our plan for testing is to test as many parts of the codebase
  using automated OUnit testing. We tested components as they were built. For
  example, one of our first sprints included the creation of the
  chraracter.ml/character.mli files to represent a sprite in our RPG. We tested
  that subsection before using the character.ml representation, in our battle
  simulation, whose functionality is mostly under battle.ml/battle.mli*)

(*Manually Tested Componenets *)

(*OUnit Tested Componenets : character.ml/character.mi -We utilized OUnit and
  glass box testing in order to throughly test that our implementation of
  character worked as expected. Since, we predicted that sprites would be used
  throughout our game, we made sure to be especially careful in testing the
  getters/setter of the character's attributes *)

(*Arugment for correctness of program given testing strategy: *)

let demon1 : Character.character = Character.start_character "Demoman"
let demon1_adjust = Character.adjust 10. demon1 "hp"
let demon2_adjust = Character.adjust 10. demon1 "mana"
let demon3_adjust = Character.adjust 10. demon1 "strength"
let demon4_adjust = Character.adjust 10. demon1 "defense"
let demon5_adjust = Character.adjust 10. demon1 "magic resist"
let demon6_adjust = Character.adjust 10. demon1 "speed"
let demon7_adjust = Character.adjust 10. demon1 "accuracy"
let demon8_adjust = Character.adjust 10. demon1 "magic power"
let demon9_adjust = Character.adjust 10. demon1 "luck"
let start_test (name : string) : test = name >:: fun _ -> assert_equal true true

let get_attribute_test (name : string) (character : Character.character)
    (att : string) (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output (Character.get_attribute att character)

let basic_charcter_tests =
  [
    get_attribute_test "initial hp of default chracter" demon1 "hp" 100.;
    get_attribute_test "initial mana of default chracter" demon1 "mana" 100.;
    get_attribute_test "initial strength of default chracter" demon1 "strength"
      10.;
    get_attribute_test "initial defense of default chracter" demon1 "defense"
      10.;
    get_attribute_test "initial magic_resist of default chracter" demon1
      "magic resist" 10.;
    get_attribute_test "initial accuracy of default chracter" demon1 "accuracy"
      10.;
    get_attribute_test "initial magic_power of default chracter" demon1 "magic"
      10.;
    get_attribute_test "initial luck of default chracter" demon1 "luck" 10.;
    get_attribute_test "adjusted by 10  hp of default chracter" demon1_adjust
      "hp" 110.;
    get_attribute_test "adjusted by 10  mana of default chracter" demon2_adjust
      "mana" 110.;
    get_attribute_test "adjusted by 10  strength of default chracter"
      demon3_adjust "strength" 20.;
    get_attribute_test "adjusted by 10  defense of default chracter"
      demon4_adjust "defense" 20.;
    get_attribute_test "adjusted by 10  magic_resist of default chracter"
      demon5_adjust "magic resist" 20.;
    get_attribute_test "adjusted by 10  accuracy of default chracter"
      demon7_adjust "accuracy" 20.;
    get_attribute_test "adjusted by 10  magic_power of default chracter"
      demon8_adjust "magic" 20.;
    get_attribute_test "adjusted by 10  luck of default chracter" demon9_adjust
      "luck" 20.;
  ]

let test_suite =
  "test suite for Final Project" >::: List.flatten [ basic_charcter_tests ]

let _ = run_test_tt_main test_suite