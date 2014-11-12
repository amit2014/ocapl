all:
	ocamlc -c op.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c controller.ml
	ocamlc -o ocapl lexer.cmo parser.cmo controller.cmo op.cmo
	rm -f lexer.ml *.cmo *.cmi parser.ml parser.mli 2> /dev/null
clean:
	rm -f lexer.ml *.cmo *.cmi parser.ml parser.mli ocapl 2> /dev/null
preserve:
	ocamlc -c op.ml
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c controller.ml
	ocamlc -o ocapl lexer.cmo parser.cmo controller.cmo op.cmo
	
