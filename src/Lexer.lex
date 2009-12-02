{
 open Lexing;

 exception LexicalError of string * (int * int) (* (message, (line, column)) *)

 val currentLine = ref 1
 val lineStartPos = ref [0]

 fun getPos lexbuf = getLineCol (getLexemeStart lexbuf)
				(!currentLine)
				(!lineStartPos)

 and getLineCol pos line (p1::ps) =
       if pos>=p1 then (line, pos-p1)
       else getLineCol pos (line-1) ps
   | getLineCol pos line [] = raise LexicalError ("",(0,0))

 fun lexerError lexbuf s = 
     raise LexicalError (s, getPos lexbuf)

 fun keyword (s, pos) =
     case s of
         "procedure"    => Parser.PROCEDURE pos
       | "skip"         => Parser.SKIP pos
       | "call"         => Parser.CALL pos
       | "uncall"	=> Parser.UNCALL pos
       | "with"         => Parser.WITH pos
       | "if"		=> Parser.IF pos
       | "then"		=> Parser.THEN pos
       | "else"		=> Parser.ELSE pos
       | "fi"		=> Parser.FI pos
       | "from"		=> Parser.FROM pos
       | "do"		=> Parser.DO pos
       | "loop"		=> Parser.LOOP pos
       | "until"	=> Parser.UNTIL pos
       | _              => Parser.ID (s, pos)

 }

rule Token = parse
    [` ` `\t` `\r`]+    { Token lexbuf } (* whitespace *)
    | "//" [^`\n`]*	{ Token lexbuf } (* comment *)
  | [`\n` `\012`]       { currentLine := !currentLine+1;
                          lineStartPos :=  getLexemeStart lexbuf
			                   :: !lineStartPos;
                          Token lexbuf } (* newlines *)
  | [`0`-`9`]+          { case Int.fromString (getLexeme lexbuf) of
                               NONE   => lexerError lexbuf "Bad integer"
                             | SOME i => Parser.NUM (i, getPos lexbuf) }
  | [`a`-`z``A`-`Z`] [`a`-`z``A`-`Z``0`-`9``_`]*
                        { keyword (getLexeme lexbuf,getPos lexbuf) }
  | `+`                 { Parser.PLUS (getPos lexbuf) }
  | `-`                 { Parser.MINUS (getPos lexbuf) }
  | "/2"                { Parser.HALF (getPos lexbuf) }
  | "-="                { Parser.SUBTRACT (getPos lexbuf) }
  | "->"                { Parser.ARROW (getPos lexbuf) }
  | "+="                { Parser.ADD (getPos lexbuf) }
  | `(`                 { Parser.LPAR (getPos lexbuf) }
  | `)`                 { Parser.RPAR (getPos lexbuf) }
  | `[`                 { Parser.LBRACK (getPos lexbuf) }
  | `]`                 { Parser.RBRACK (getPos lexbuf) }
  | "=="                { Parser.EQ (getPos lexbuf) }
  | `<`                 { Parser.LESS (getPos lexbuf) }
  | `!`                 { Parser.NOT (getPos lexbuf) }
  | "&&"                { Parser.AND (getPos lexbuf) }
  | "||"                { Parser.OR (getPos lexbuf) }
  | `;`                 { Parser.SEMICOLON (getPos lexbuf) }
  | eof                 { Parser.EOF (getPos lexbuf) }
  | _                   { lexerError lexbuf "Illegal symbol in input" }
;
