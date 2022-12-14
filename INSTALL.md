# Install CornQuest

First, install OCaml and set it up as the CS3110 [textbook](https://cs3110.github.io/textbook/chapters/preface/install.html) specifies.

Next, ensure `make` is installed. Try `make --version`:

```shell
GNU Make 4.2.1
Built for x86_64-pc-linux-gnu
Copyright (C) 1988-2016 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
```

For Ubuntu (Linux or WSL with Ubuntu users, Windows 11 users may ignore this step):
  - Install a window server that supports the X Window System. You can find  more information on setting this up, and a list of window servers [here](https://wiki.ubuntu.com/WSL#Running_Graphical_Applications)
  - Navigate to "~./bshrc" in your home directory and add these following lines of code:

  ```
  export DISPLAY=:0 # in WSL 1
  export DISPLAY=$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null):0 # in WSL 2
  export LIBGL_ALWAYS_INDIRECT=1
  ```
  - Before running the game on terminal, start your window server of choice, and at launch tick off "disable access control"
  - If X Window Server will not run on your computer, or if the GUI does not open despite following the above steps, it is recommended you run this program in the OCaml VM environment that you can find in the CS3110 [textbook](https://cs3110.github.io/textbook/chapters/appendix/vm.html?highlight=virtual%20machine).

Now, install `sdl2` for your operating system.

- On macOS (Source: Homebrew install instructions):
  - Homebrew: 
    - `brew install pkgconfig`
    - `brew install sdl2`
    - `brew install sdl2_image`
    - `brew install sdl2_mixer`
    - `brew install sdl2_ttf`
  - MacPorts:
    - `port install pkgconfig`
    - `port install libsdl2`

- On Ubuntu (or WSL with Ubuntu):
  - `sudo apt-get update`
  - `sudo apt-get install libffi-dev`
  - `sudo apt-get install libsdl2-dev`
  - `sudo apt-get install libsdl2-image-dev`
  - `sudo apt-get install libsdl2-mixer-dev`
  - You may need to also install `pkgconfig` with `sudo apt-install pkgconfig`

Next, install the required dependencies using `opam`:

```
$ opam install ANSITerminal
$ opam install tsdl-image
$ opam install tsdl-mixer
```

You have finished installing CornQuest!

To run the program, run `make play`. This will launch the game window and load up the battle system in terminal. Open the terminal in fullscreen for the best experience!

If you encounter an issue while running `make play` it may be because your version of dune is outdated. To update, run:
```
$ opam update
$ opam upgrade
```
