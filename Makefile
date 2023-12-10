
run:
	OCAMLRUNPARAM=b dune exec Startup/main.exe

backtest:
	OCAMLRUNPARAM=b dune exec backtesting/backtest.exe

zip:
	rm -f psi_capital.zip
	zip -r psi_capital.zip .

clean: 
	dune clean

build:
	dune build

poop:
	OCAMLRUNPARAM=b dune exec Startup/main.exe

cloc:
	OCAMLRUNPARAM=b cloc --by-file --include-lang=OCaml .

doc:
	dune build @doc

openuserdoc: doc
	@bash opendoc.sh User

openoptionsdoc: doc
	@bash opendoc.sh Options

openbacktestdoc: doc
	@bash opendoc.sh backtest


