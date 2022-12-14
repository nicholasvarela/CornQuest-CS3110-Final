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
	OCAMLRUNPARAM=b dune exec bin/demo_battle.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

test: 
		OCAMLRUNPARAM=b dune exec test/test.exe

zip:
	rm -f cornquest.zip
	zip -r cornquest.zip . -x@exclude.lst
	
doc:
	dune build @doc
