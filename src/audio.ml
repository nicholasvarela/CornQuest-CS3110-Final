(* open Tsdl_mixer

(**Sample size used for audio playback.*)
let samp_size = 2048

type t = (int, Mixer.music) Hashtbl.t

let init () : t =
  Mixer.open_audio Mixer.default_frequency Mixer.default_format
    Mixer.default_channels samp_size
  |> Result.get_ok;
  Hashtbl.create 4

let load_music (sys : t) fl id =
  Hashtbl.replace sys id (Mixer.load_mus fl |> Result.get_ok)

let close (sys : t) =
  Hashtbl.iter (fun _ mus -> Mixer.free_music mus) sys;
  Mixer.quit ()

let play (sys : t) id =
  if not (Mixer.playing_music ()) then
    match Mixer.play_music (Hashtbl.find sys id) (-1) |> Result.get_ok with
    | 0 -> ()
    | 1 -> failwith "Failed to play music."
    | _ -> failwith "unreachable"
  else (
    Mixer.halt_music () |> Result.get_ok;
    match Mixer.play_music (Hashtbl.find sys id) (-1) |> Result.get_ok with
    | 0 -> ()
    | 1 -> failwith "Failed to play music."
    | _ -> failwith "unreachable")
TODO: Pause the current track when [play] is called instead of killing it *)