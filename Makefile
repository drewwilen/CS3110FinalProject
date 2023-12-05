
run:
	OCAMLRUNPARAM=b dune exec Startup/main.exe

zip:
	rm -f psi_capital.zip
	zip -r psi_capital.zip .

clean: 
	dune clean

build:
	dune build


