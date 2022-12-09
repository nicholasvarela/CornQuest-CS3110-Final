.PHONY: test

build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

battle:
	OCAMLRUNPARAM=b dune exec bin/battle_main.exe

gui:
	OCAMLRUNPARAM=b dune exec bin/gui_main.exe

test: 
		OCAMLRUNPARAM=b dune exec test/test.exe

zip:
	rm -f cornquest.zip
	zip -r cornquest.zip . -x@exclude.lst

