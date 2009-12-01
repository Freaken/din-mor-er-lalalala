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

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";

val yytransl = #[
  257 (* ADD *),
  258 (* ARROW *),
  259 (* CALL *),
  260 (* EOF *),
  261 (* HALF *),
  262 (* ID *),
  263 (* LPAR *),
  264 (* MINUS *),
  265 (* NUM *),
  266 (* PLUS *),
  267 (* PROCEDURE *),
  268 (* RPAR *),
  269 (* SEMICOLON *),
  270 (* SKIP *),
  271 (* SUBTRACT *),
  272 (* WITH *),
    0];

val yylhs = "\255\255\
\\001\000\001\000\002\000\002\000\003\000\003\000\004\000\004\000\
\\004\000\004\000\004\000\005\000\005\000\005\000\005\000\005\000\
\\005\000\006\000\000\000";

val yylen = "\002\000\
\\009\000\007\000\002\000\000\000\004\000\000\000\003\000\003\000\
\\003\000\001\000\002\000\001\000\001\000\003\000\003\000\002\000\
\\003\000\001\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\000\000\019\000\000\000\003\000\000\000\000\000\
\\000\000\000\000\000\000\018\000\010\000\000\000\000\000\000\000\
\\011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\002\000\000\000\012\000\000\000\013\000\000\000\000\000\
\\000\000\000\000\016\000\000\000\000\000\000\000\005\000\017\000\
\\000\000\000\000\001\000";

val yydgoto = "\002\000\
\\004\000\005\000\020\000\014\000\029\000\030\000";

val yysindex = "\018\000\
\\251\254\000\000\251\254\000\000\039\255\000\000\251\254\249\254\
\\017\255\251\254\045\255\000\000\000\000\052\255\007\255\044\255\
\\000\000\060\255\017\255\054\255\055\255\055\255\017\255\017\255\
\\056\255\000\000\055\255\000\000\019\255\000\000\019\255\052\255\
\\052\255\042\255\000\000\055\255\055\255\063\255\000\000\000\000\
\\065\255\065\255\000\000";

val yyrindex = "\000\000\
\\066\255\000\000\005\255\000\000\000\000\000\000\012\255\000\000\
\\000\000\058\255\000\000\000\000\000\000\068\255\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\255\254\000\000\000\000\000\000\000\255\000\000\035\255\068\255\
\\068\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\022\255\032\255\000\000";

val yygindex = "\000\000\
\\000\000\046\000\005\000\036\000\234\255\249\255";

val YYTABLESIZE = 72;
val yytable = "\031\000\
\\003\000\015\000\007\000\008\000\034\000\009\000\004\000\021\000\
\\010\000\007\000\008\000\015\000\008\000\041\000\042\000\015\000\
\\015\000\004\000\001\000\011\000\004\000\022\000\012\000\035\000\
\\004\000\015\000\036\000\004\000\037\000\015\000\013\000\015\000\
\\015\000\015\000\015\000\014\000\038\000\039\000\009\000\014\000\
\\007\000\014\000\014\000\014\000\014\000\009\000\035\000\009\000\
\\006\000\036\000\017\000\037\000\008\000\040\000\025\000\016\000\
\\023\000\026\000\032\000\033\000\012\000\027\000\018\000\028\000\
\\019\000\024\000\043\000\004\000\019\000\035\000\004\000\006\000";

val yycheck = "\022\000\
\\006\001\009\000\004\001\004\001\027\000\013\001\002\001\001\001\
\\016\001\011\001\011\001\019\000\013\001\036\000\037\000\023\000\
\\024\000\013\001\001\000\003\001\016\001\015\001\006\001\005\001\
\\013\001\004\001\008\001\016\001\010\001\008\001\014\001\010\001\
\\011\001\012\001\013\001\004\001\032\000\033\000\004\001\008\001\
\\002\001\010\001\011\001\012\001\013\001\011\001\005\001\013\001\
\\003\000\008\001\006\001\010\001\007\000\012\001\019\000\010\000\
\\013\001\004\001\023\000\024\000\006\001\007\001\011\001\009\001\
\\013\001\006\001\004\001\002\001\013\001\005\001\013\001\004\001";

val yyact = vector_ 20 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file Parser.grm, line 30 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 8 : Janus.Def list
val d__2__ = peekVal 7 : (int*int)
val d__3__ = peekVal 6 : Janus.Def list
val d__4__ = peekVal 5 : (int*int)
val d__5__ = peekVal 4 : Janus.Def list
val d__6__ = peekVal 3 : (int*int)
val d__7__ = peekVal 2 : Janus.Stat
val d__8__ = peekVal 1 : Janus.Procedure list
val d__9__ = peekVal 0 : (int*int)
in
( ((d__1__),(d__3__),(d__5__),(d__7__),(d__8__)) ) end : Janus.Prog))
;
(* Rule 2, file Parser.grm, line 32 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 6 : Janus.Def list
val d__2__ = peekVal 5 : (int*int)
val d__3__ = peekVal 4 : Janus.Def list
val d__4__ = peekVal 3 : (int*int)
val d__5__ = peekVal 2 : Janus.Stat
val d__6__ = peekVal 1 : Janus.Procedure list
val d__7__ = peekVal 0 : (int*int)
in
( ((d__1__),(d__3__),[],(d__5__),(d__6__)) ) end : Janus.Prog))
;
(* Rule 3, file Parser.grm, line 35 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__1__ = peekVal 1 : string*(int*int)
val d__2__ = peekVal 0 : Janus.Def list
in
( Janus.IntVarDef (d__1__) :: (d__2__) ) end : Janus.Def list))
;
(* Rule 4, file Parser.grm, line 36 *)
val _ = update_ yyact 4
(fn () => repr(let
in
( [] ) end : Janus.Def list))
;
(* Rule 5, file Parser.grm, line 40 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : string*(int*int)
val d__3__ = peekVal 1 : Janus.Stat
val d__4__ = peekVal 0 : Janus.Procedure list
in
( (#1 (d__2__), (d__3__), (d__1__)) :: (d__4__) ) end : Janus.Procedure list))
;
(* Rule 6, file Parser.grm, line 41 *)
val _ = update_ yyact 6
(fn () => repr(let
in
( [] ) end : Janus.Procedure list))
;
(* Rule 7, file Parser.grm, line 45 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 2 : Janus.Stat
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Janus.Stat
in
( Janus.Sequence ((d__1__), (d__3__), (d__2__)) ) end : Janus.Stat))
;
(* Rule 8, file Parser.grm, line 46 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 2 : Janus.Lval
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Janus.Exp
in
( Janus.AddUpdate ((d__1__), (d__3__), (d__2__)) ) end : Janus.Stat))
;
(* Rule 9, file Parser.grm, line 48 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__1__ = peekVal 2 : Janus.Lval
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Janus.Exp
in
( Janus.SubUpdate ((d__1__), (d__3__), (d__2__)) ) end : Janus.Stat))
;
(* Rule 10, file Parser.grm, line 49 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Janus.Skip (d__1__) ) end : Janus.Stat))
;
(* Rule 11, file Parser.grm, line 50 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 1 : (int*int)
val d__2__ = peekVal 0 : string*(int*int)
in
( Janus.Call (#1 (d__2__), (d__1__)) ) end : Janus.Stat))
;
(* Rule 12, file Parser.grm, line 54 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 0 : int*(int*int)
in
( Janus.Num (d__1__) ) end : Janus.Exp))
;
(* Rule 13, file Parser.grm, line 55 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 0 : Janus.Lval
in
( Janus.LVal (d__1__) ) end : Janus.Exp))
;
(* Rule 14, file Parser.grm, line 56 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 2 : Janus.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Janus.Exp
in
( Janus.Plus ((d__1__), (d__3__), (d__2__)) ) end : Janus.Exp))
;
(* Rule 15, file Parser.grm, line 57 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 2 : Janus.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Janus.Exp
in
( Janus.Minus ((d__1__), (d__3__), (d__2__)) ) end : Janus.Exp))
;
(* Rule 16, file Parser.grm, line 58 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 1 : Janus.Exp
val d__2__ = peekVal 0 : (int*int)
in
( Janus.Half ((d__1__), (d__2__)) ) end : Janus.Exp))
;
(* Rule 17, file Parser.grm, line 59 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Janus.Exp
val d__3__ = peekVal 0 : (int*int)
in
( (d__2__) ) end : Janus.Exp))
;
(* Rule 18, file Parser.grm, line 62 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( Janus.IntVar (d__1__) ) end : Janus.Lval))
;
(* Entry Prog *)
val _ = update_ yyact 19 (fn () => raise yyexit (peekVal 0));
val yytables : parseTables =
  ( yyact,
    yytransl,
    yylhs,
    yylen,
    yydefred,
    yydgoto,
    yysindex,
    yyrindex,
    yygindex,
    YYTABLESIZE,
    yytable,
    yycheck );
fun Prog lexer lexbuf = yyparse yytables 1 lexer lexbuf;
