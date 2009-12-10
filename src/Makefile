# PATH := $(PATH):/home/freaken/mosml/bin/

all:
	cd ..; make src

src: JC

%.ui: %.sml

RegAlloc.ui: Mips.ui
Type.ui: Janus.ui
Compiler.ui: Janus.ui Mips.ui

JC: Mips.ui RegAlloc.ui Janus.ui Parser.ui Lexer.ui Type.ui Compiler.ui
	mosmlc -o JC JC.sml

Mips.ui:
	mosmlc -c Mips.sml

Janus.ui:
	mosmlc -c Janus.sml

Lexer.ui: Lexer.sml
	mosmlc -c Lexer.sml

Lexer.sml: Lexer.lex
	mosmllex Lexer.lex

Parser.sml: Parser.grm
	mosmlyac -v Parser.grm

%.ui: %.sml
	mosmlc -c $*.sig $*.sml

clean:
	rm -f *.ui *.uo JC
	rm -f Parser.sig Parser.sml Parser.output
	rm -f Lexer.sml

