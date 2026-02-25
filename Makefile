test: FORCE
	cd src; dune build && ./tester.exe

debug: FORCE
	cd src; dune build && OCAMLRUNPARAM=b ./tester.exe

FORCE:
