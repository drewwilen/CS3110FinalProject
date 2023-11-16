
run:
	OCAMLRUNPARAM=b dune exec Scraping/main.exe

zip:
	rm -f psi_capital.zip
	zip -r psi_capital.zip .

clean: 
	dune clean
