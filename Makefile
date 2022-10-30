all: aritha rapport

aritha:
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlc asyntax.ml parser.mli parser.ml lexer.ml compile.ml main.ml -o aritha
rapport:
	pdflatex rapport.tex

clean:
	rm -rf *.cmo *.cmi *.cmx *.o *.s *.pdf *.aux *.out *.log parser.ml parser.mli lexer.ml aritha