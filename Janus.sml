structure Janus =
struct

  (* types for abstract syntax for Janus *)

  type pos = int * int  (* position: line, column *)

  datatype Lval
    = IntVar of string * pos
    | ArrayIndex of string * Exp * pos

  and Exp 
    = Num of int * pos
    | LVal of Lval
    | Plus of Exp * Exp * pos
    | Minus of Exp * Exp * pos
    | Half of Exp * pos

  datatype Cond 
    = Equal of Exp * Exp * pos
    | Less of Exp * Exp * pos
    | Not of Cond * pos
    | And of Cond * Cond * pos
    | Or of Cond * Cond * pos

  datatype Stat
    = Sequence of Stat * Stat * pos
    | AddUpdate of Lval * Exp * pos
    | SubUpdate of Lval * Exp * pos
    | If of Cond * Stat * Stat * Cond * pos
    | Loop of Cond * Stat * Stat * Cond * pos
    | Skip of pos
    | Call of string * pos
    | Uncall of string * pos

  datatype Def
    = IntVarDef of string * pos
    | ArrayVarDef of string * int * pos

  type Procedure = string * Stat * pos

  type Prog = Def list * Def list * Def list * Stat * Procedure list

end
