(**Loading and management of game textures.*)

val load_texture : string -> Tsdl.Sdl.renderer -> Tsdl.Sdl.texture
(**[load_texture file_name ren] loads the texture from the file [file_name]
   using the renderer [ren].*)

val draw :
  Tsdl.Sdl.texture ->
  Tsdl.Sdl.renderer ->
  Tsdl.Sdl.rect ->
  Tsdl.Sdl.rect ->
  unit
(**[draw tex ren src dst] draws the part of the texture [tex] defined by [src]
   using renderer [ren] on the part of the screen defined by [dst].*)
