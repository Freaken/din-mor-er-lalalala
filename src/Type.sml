structure Type :> Type =
struct

  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*(int*int)

  datatype JanusType = Integer | Array of int

  type pos = int*int

  (* lookup function for symbol table as list of (name,value) pairs *)
  fun lookup x []
        = NONE
    | lookup x ((y,v)::table)
        = if x=y then SOME v else lookup x table

  (* check expression *)
  fun checkExp exp vtable avoid =
  (* avoid is the LHS variable that can not be used on the RHS *)
    case exp of
      Janus.Num (n,pos) => ()
    | Janus.LVal lv =>
       (case lv of
           Janus.IntVar (x,p) =>
             (case lookup x vtable of
               SOME Integer =>
                 if x=avoid
                 then raise Error ("LHS variable used on RHS",p)
                 else ()
              | SOME (Array _) => raise Error ("Variable " ^ x ^ " is an Array, but is used as an Integer", p)
              | _ => raise Error ("Variable " ^ x ^ " is not defined",p))
        |  Janus.ArrayIndex (x,e,p) =>
            (checkExp e vtable avoid;
                (case lookup x vtable of
                  SOME (Array _) =>
                    if x=avoid
                    then raise Error ("LHS variable used on RHS",p)
                    else ()
                | SOME Integer => raise Error ("Variable " ^ x ^ " is an Integer, but is used as an Array", p)
                | _ => raise Error ("Variable " ^ x ^ " is not defined",p)))
       )
    | Janus.Plus (e1,e2,pos) =>
       (checkExp e1 vtable avoid; checkExp e2 vtable avoid)
    | Janus.Minus (e1,e2,pos) =>
       (checkExp e1 vtable avoid; checkExp e2 vtable avoid)
    | Janus.Half (e1,pos) => checkExp e1 vtable avoid

  (* Check condition *)
  fun checkCond c vtable =
    case c of
         Janus.Equal(e1,e2,p) => (checkExp e1 vtable ""; checkExp e2 vtable "")
       | Janus.Less (e1,e2,p) => (checkExp e1 vtable ""; checkExp e2 vtable "")
       | Janus.Not  (c1,p)    => (checkCond c1 vtable)
       | Janus.And  (c1,c2,p) => (checkCond c1 vtable; checkCond c2 vtable)
       | Janus.Or   (c1,c2,p) => (checkCond c1 vtable; checkCond c2 vtable)

  fun checkStat s vtable pnames =
    case s of
      Janus.Sequence (s1,s2,pos) =>
        (checkStat s1 vtable pnames; checkStat s2 vtable pnames)
    | Janus.AddUpdate (lv,e,pos) =>
        (case lv of
           Janus.IntVar (x,p) =>
             (case lookup x vtable of
               SOME Integer =>
                 checkExp e vtable x
             | SOME (Array _) => raise Error ("Variable " ^ x ^ " is an Array, but is used as an Integer", p)
             | _ => raise Error ("Variable " ^ x ^ " is not defined",p))
        |  Janus.ArrayIndex (x,e1,p) =>
             (checkExp e1 vtable "";
             (case lookup x vtable of
               SOME (Array _) =>
                 checkExp e vtable x
             | SOME Integer => raise Error ("Variable " ^ x ^ " is an Integer, but is used as an Array", p)
             | _ => raise Error ("Variable " ^ x ^ " is not defined",p)))
        )
    | Janus.SubUpdate (lv,e,pos) =>
        (case lv of
           Janus.IntVar (x,p) =>
             (case lookup x vtable of
               SOME Integer =>
                 checkExp e vtable x
             | SOME (Array _) => raise Error ("Variable " ^ x ^ " is an Array, but is used as an Integer", p)
             | _ => raise Error ("Variable " ^ x ^ " is not defined",p))
        |  Janus.ArrayIndex (x,e1,p) =>
             (checkExp e1 vtable "";
             (case lookup x vtable of
               SOME (Array _) =>
                 checkExp e vtable x
             | SOME Integer => raise Error ("Variable " ^ x ^ " is an Integer, but is used as an Array", p)
             | _ => raise Error ("Variable " ^ x ^ " is not defined",p)))
        )
    | Janus.If (c1,s1,s2,c2,pos) =>
        (
         checkCond c1 vtable       ; checkStat s1 vtable pnames;
         checkStat s2 vtable pnames; checkCond c2 vtable
        )
    | Janus.Loop (c1,s1,s2,c2,pos) =>
        (
         checkCond c1 vtable       ; checkStat s1 vtable pnames;
         checkStat s2 vtable pnames; checkCond c2 vtable
        )
    | Janus.Skip pos => ()
    | Janus.Call (p,pos) =>
        if List.exists (fn q=>q=p) pnames
        then ()
        else raise Error ("Unknown procedure "^p,pos)
    | Janus.Uncall (p,pos) =>
        if List.exists (fn q=>q=p) pnames
        then ()
        else raise Error ("Unknown procedure "^p,pos)

  fun checkDefs [] vtable = vtable
    | checkDefs (Janus.IntVarDef (x,pos)::defs) vtable =
        (case lookup x vtable of
           NONE => checkDefs defs ((x,Integer)::vtable)
         | SOME _ => raise Error ("Multiple declaration of "^x,pos))
    | checkDefs (Janus.ArrayVarDef (x,size,pos)::defs) vtable =
        if size = 0 then raise Error ("Zero-sized array",pos)
        else
          (case lookup x vtable of
             NONE => checkDefs defs ((x,Array size )::vtable)
           | SOME _ => raise Error ("Multiple declaration of "^x,pos))

  fun getProcs [] pnames = pnames
    | getProcs ((p,s,pos)::procs) pnames =
        if List.exists (fn q=>q=p) pnames
        then raise Error ("Multiply declared procedure "^p,pos)
        else getProcs procs (p::pnames)

  fun checkProcs [] vtable pnames = ()
    | checkProcs ((p,s,pos)::procs) vtable pnames =
        (checkStat s vtable pnames; checkProcs procs vtable pnames)

  (* main function for checking program *)
  fun checkProgram (ins,outs,locals,stat,procs) =
    let
      val vtable0 = checkDefs ins []
      val vtable1 = checkDefs outs vtable0
      val vtable2 = checkDefs locals vtable1
      val pnames = getProcs procs []
      val () = checkProcs procs vtable2 pnames
    in
      checkStat stat vtable2 pnames
    end

end
