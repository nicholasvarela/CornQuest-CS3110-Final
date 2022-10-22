open Game
open Battle

let actor : Character.character = Character.start_character "Demoman"

let enem : Character.character =
  {
    name = "Marthia Pollocus";
    hp = HP 100.;
    mana = Mana 100.;
    exp = 0.;
    lvl = 1;
    str = Strength 10.;
    def = Defense 10.;
    mr = MagicResist 10.;
    spd = Speed 10.;
    acc = Accuracy 10.;
    mag = MagicPower 10.;
    luk = Luck 10.;
    enem_hit_chances = [ 1. ];
    skillset = [];
    temp_stats = [];
  }