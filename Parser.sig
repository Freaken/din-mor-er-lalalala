local
type t__1__ = (int*int)
type t__2__ = (int*int)
type t__3__ = (int*int)
type t__4__ = (int*int)
type t__5__ = (int*int)
type t__6__ = string*(int*int)
type t__7__ = (int*int)
type t__8__ = (int*int)
type t__9__ = int*(int*int)
type t__10__ = (int*int)
type t__11__ = (int*int)
type t__12__ = (int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = (int*int)
type t__16__ = (int*int)
in
datatype token =
    ADD of t__1__
  | ARROW of t__2__
  | CALL of t__3__
  | EOF of t__4__
  | HALF of t__5__
  | ID of t__6__
  | LPAR of t__7__
  | MINUS of t__8__
  | NUM of t__9__
  | PLUS of t__10__
  | PROCEDURE of t__11__
  | RPAR of t__12__
  | SEMICOLON of t__13__
  | SKIP of t__14__
  | SUBTRACT of t__15__
  | WITH of t__16__
end;

val Prog :
  (Lexing.lexbuf -> token) -> Lexing.lexbuf -> Janus.Prog;
