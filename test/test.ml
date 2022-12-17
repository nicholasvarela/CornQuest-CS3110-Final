open OUnit2
open Game
open Yojson

(*Testing Plan: Our plan for testing is to test as many parts of the codebase
  using automated OUnit testing. We tested components as they were built. For
  example, one of our first sprints included the creation of the [Character] to
  represent a sprite in our RPG. We tested that subsection before using the
  [Character] representation in our battle simulation, whose functionality is
  mostly under [Battle].*)

(*Manually Tested Components: Attacks in the battle systems [Battle] have a
  random roll, so they could not be accurately tested with OUnit battle.ml
  functions. We focused on testing functions that were exposed in the respective
  .mli files, internal functions were solved indirectly. We also manually tested
  GUI Components like [Player], [Tilemap], [Tileset] and [Textman]. We were
  advised by our PM that we watch out for the game crashing with unusual input.
  Therefore, we manually tested unsual inputs in terminal while running the demo
  battle system. *)

(*OUnit Tested Componenets : [Character] -We utilized OUnit and glass box
  testing in order to throughly test that our implementation of character worked
  as expected. Since, we predicted that sprites would be used throughout our
  game, we made sure to be especially careful in testing the getters/setter of
  the character's attributes. [Character] is the only non-GUI, non-[Battle]
  module that truly modifies the state of the program, as this module passes a
  new character into the new state in the GUI. Employing OUnit testing here was
  a strategic choice and helped immensely in ensuring as much correctness as
  possible.*)

(*Argument for correctness of program given testing strategy:

  Our testing method proves correctness of our program because it rigorouly
  tests each subcomponenet of our project. Through Glass Box and Black Box
  testing we ensured that functions produced output as prescribed by their
  respective .mli files. This ensured that functions which utilized their
  already produced functions only had to be tested for new functionality. Aside
  from that, all components of the program that could possibly change the state
  of the game have been tested for errors, proving the correctness of
  Cornquest's battle functionality. For the GUI aspect of the game, as only
  movement and encounters have been implemented at this point in time, it is
  fairly straightforward to see if things are not working as intended, hence we
  rely on manual testing a lot here.

  In sum, the [Battle] and [Character] modules have been thouroughly tested by
  OUnit, and GUI components were tested manually. Since the former two modules
  modify the state by passing a new player character into the new state, given
  that the former two have been tested to be correct, it is sufficient to say
  that the program has been sufficiently tested.*)

let demon1 : Character.character = Character.start_character "Demoman"
let chris : Character.character = Character.start_character "Chris"
let chris2 = Character.adjust_temps (Defense 2.0, 0) chris
let chris3 = Character.adjust_temps (MagicResist 2.0, 0) chris
let chris4 = Character.adjust_temps (Luck 2.0, 0) chris
let chris5 = Character.adjust_temps (Speed 2.0, 0) chris
let daniel : Character.character = Character.start_character "Daniel"
let daniel1 = Character.adjust_temps (Defense (-3.0), 0) daniel
let daniel2 = Character.adjust_temps (MagicResist (-3.0), 0) daniel
let daniel3 = Character.adjust_temps (Luck (-3.0), 0) daniel
let daniel4 = Character.adjust_temps (Speed (-3.0), 0) daniel
let demon1_adjust = Character.adjust 10. demon1 "hp"
let demon2_adjust = Character.adjust 10. demon1 "mana"
let demon3_adjust = Character.adjust 10. demon1 "strength"
let demon4_adjust = Character.adjust 10. demon1 "defense"
let demon5_adjust = Character.adjust 10. demon1 "magic resist"
let demon6_adjust = Character.adjust 10. demon1 "speed"
let demon7_adjust = Character.adjust 10. demon1 "accuracy"
let demon8_adjust = Character.adjust 10. demon1 "magic power"
let demon9_adjust = Character.adjust 10. demon1 "luck"
let martha = Character.parse_character "Marthia Pollocus" []
let serpent = Character.parse_character "Hovian Plaza Serpent" []
let start_test (name : string) : test = name >:: fun _ -> assert_equal true true

let get_attribute_test (name : string) (character : Character.character)
    (att : string) (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output (Character.get_attribute_val att character)

let get_name_test (name : string) (character : Character.character)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Character.get_name character)

let get_temp_value_test (name : string) (character : Character.character)
    (attr : string) (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output (Character.get_temp_value attr character)
    ~printer:(fun x -> string_of_float x)

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
      1000.;
    get_attribute_test "initial magic_power of default chracter" demon1
      "magic power" 10.;
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
      demon7_adjust "accuracy" 1010.;
    get_attribute_test "adjusted by 10  magic_power of default chracter"
      demon8_adjust "magic power" 20.;
    get_attribute_test "adjusted by 10  luck of default chracter" demon9_adjust
      "luck" 20.
    (* get_curr_attr_test "get_curr_attr hp default character" demon1 "maxmana"
       100.; *);
    get_name_test "name of chris is chris" chris "Chris";
    get_name_test "name of daniel is Daniel" daniel "Daniel";
    get_name_test "name of chris is chris" demon1 "Demoman";
    get_temp_value_test "initial tmp of maxhp of daemon " demon1 "maxhp" 0.;
    get_temp_value_test "initial tmp of maxmana of daemon " demon1 "maxmana" 0.;
    get_temp_value_test "initial tmp of strength of daemon " demon1 "strength"
      0.;
    get_temp_value_test "initial tmp of defense of daemon " demon1 "defense" 0.;
    get_temp_value_test "initial tmp of magic reiss of daemon " demon1
      "magic resist" 0.;
    get_temp_value_test "initial tmp of speed of daemon " demon1 "speed" 0.;
    get_temp_value_test "initial tmp of magic power of daemon " demon1
      "magic power" 0.;
    get_temp_value_test "change temp luck" chris "luck" 2.;
    get_temp_value_test "change temp luck" chris "luck" 2.;
    get_temp_value_test "change temp defenasdfse" chris2 "defense" 2.;
    get_temp_value_test "change temp luck" chris4 "luck" 2.;
    get_temp_value_test "change temp speed" chris5 "speed" 2.;
    get_temp_value_test "change temp luck" daniel "luck" (-3.0);
    get_temp_value_test "change temp defense" daniel2 "defense" (-3.);
    get_temp_value_test "change temp luck" daniel4 "luck" (-3.0);
    get_attribute_test "initial hp of martha" martha "hp" 100.;
    get_attribute_test "initial mana of martha" martha "mana" 200.;
    get_attribute_test "initial strength of martha" martha "strength" 10.;
    get_attribute_test "initial defense of martha" martha "defense" 10.;
    get_attribute_test "initial magic_resist of martha" martha "magic resist"
      14.;
    get_attribute_test "initial accuracy of martha" martha "accuracy" 100000.;
    get_attribute_test "initial magic_power of martha" martha "magic power" 12.;
    get_attribute_test "initial luck of martha" martha "luck" 12.;
    get_attribute_test "initial hp of serpent" serpent "hp" 30.;
    get_attribute_test "initial mana of serpent" serpent "mana" 70.;
    get_attribute_test "initial strength of serpent" serpent "strength" 22.;
    get_attribute_test "initial defense of serpent" serpent "defense" 10.;
    get_attribute_test "initial magic_resist of serpent" serpent "magic resist"
      7.;
    get_attribute_test "initial accuracy of serpent" serpent "accuracy" 10.;
    get_attribute_test "initial magic_power of serpent" serpent "magic power"
      12.;
    get_attribute_test "initial luck of serpent" serpent "luck" 12.;
  ]

let test_suite =
  "test suite for Final Project" >::: List.flatten [ basic_charcter_tests ]

let _ = run_test_tt_main test_suite