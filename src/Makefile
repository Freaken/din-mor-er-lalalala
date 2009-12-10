# Jeg burde lave regler for både .ui og .uo filer - men
# det ville bare være spild af plads uden at gavne noget.

# Filen som konstueres med almindeliget make-kald er JC
all: JC

# Afhængigheder for filer, som bliver fanget af wildcardet
RegAlloc.ui: Mips.ui
Type.ui: Janus.ui
Compiler.ui: Janus.ui Mips.ui


# Filer med signatur bliver kompileret af wildcardet nederst.
# Filer uden signatur får en seperat regel hver.
Mips.ui: Mips.sml
	mosmlc -c Mips.sml

Janus.ui: Janus.sml
	mosmlc -c Janus.sml

Lexer.ui: Lexer.sml
	mosmlc -c Lexer.sml

Lexer.sml: Lexer.lex
	mosmllex Lexer.lex

Parser.sml: Parser.grm
	mosmlyac -v Parser.grm

JC: Mips.ui RegAlloc.ui Janus.ui Parser.ui Lexer.ui Type.ui Compiler.ui
	mosmlc -o JC JC.sml

# Den generelle regel
%.ui: %.sml
	mosmlc -c $*.sig $*.sml

# Oprydning
clean:
	rm -f *.ui *.uo JC
	rm -f Parser.sig Parser.sml Parser.output
	rm -f Lexer.sml

