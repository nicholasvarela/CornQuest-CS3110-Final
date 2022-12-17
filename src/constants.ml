(**Game-wide size of tiles. Must be a multiple of 16.*)
let tilesize = 48

(**Game-wide size of character sprite frames.*)
let spritesize = 32

(**Prefix indicating the location of the game data folder.*)
let data_dir_prefix = "data" ^ Filename.dir_sep

(**Game-wide speed of player movement.*)
let player_speed = 4

(**Width of game window.*)
let screen_width = 640

(**Height of game window.*)
let screen_height = 480

(**Target framerate for the game.*)
let framerate = 60

(**Game-wide speed of animations. Must be a divisor of [framerate]. Sprites will
   update their animations every [framerate / anim_speed] frames.*)
let anim_speed = 10
