
run:
	OCAMLRUNPARAM=b dune exec Startup/main.exe

zip:
	rm -f search.zip
	zip -r search.zip . -x@exclude.lst

clean: 
	dune clean