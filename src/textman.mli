(**Loading and management of game textures.*)

val load_texture : string -> Tsdl.Sdl.renderer -> Tsdl.Sdl.texture
(**[load_texture file_name ren] loads the texture from the file [file_name]
   using the renderer [ren].*)

val draw :
  ?offset:int * int ->
  Tsdl.Sdl.texture ->
  Tsdl.Sdl.renderer ->
  Tsdl.Sdl.rect ->
  Tsdl.Sdl.rect ->
  unit
(**[draw offset tex ren src dst] draws the part of the texture [tex] defined by
   [src] using renderer [ren] on the part of the screen defined by [dst], but
   with its position subtracted by [offset] (if supplied).*)

val draw_flipped :
  ?offset:int * int ->
  Tsdl.Sdl.texture ->
  Tsdl.Sdl.renderer ->
  Tsdl.Sdl.rect ->
  Tsdl.Sdl.rect ->
  unit
(**[draw_flipped offset tex ren src dst] horizontally flips the texture [tex],
   then draws the part of it defined by [src] using renderer [ren] on the part
   of the screen defined by [dst], but with its position subtracted by [offset]
   (if supplied).*)
