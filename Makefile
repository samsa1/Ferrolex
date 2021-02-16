LEXER = lexer.frl

all:build
	@echo "Everything built !"
build:
	@ ../Samenhir/samenhir -v2 ferrolexParser.sam
	@ ocamllex -q ferrolexLexer.mll
	@ ocamlopt ferrolexAst.ml ferrolex_var.ml ferrolex_utilities.mli ferrolex_utilities.ml ferrolexParser.mli ferrolexParser.ml ferrolexLexer.ml ferrolex.ml -o ferrolex -O3
	@ rm *.cmi *.cmx *.o ferrolexLexer.ml
	@ rm ferrolexParser.ml ferrolexParser.mli

build_timed:
	time /Users/samuel/Programmes/Samenhir/samenhir -v2 ferrolexParser.sam
	time ocamllex -q ferrolexLexer.mll
	time ocamlopt ferrolexAst.ml ferrolex_var.ml ferrolex_utilities.mli ferrolex_utilities.ml ferrolexParser.mli ferrolexParser.ml ferrolexLexer.ml ferrolex.ml -o ferrolex -O3
	time rm *.cmi *.cmx *.o ferrolexLexer.ml
	time rm ferrolexParser.ml ferrolexParser.mli

exec:
	./ferrolex $(LEXER)


clean:
	@rm -f lexer lexer.ml
	@rm -f _build/*
	@rm -f *.cmi *.o *.cmx ferrolexParser.ml ferrolexParser.mli ferrolexLexer.ml
	
cleanall: clean
	@rm -f ferrolex
