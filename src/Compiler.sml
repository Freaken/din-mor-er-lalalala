(* Compiler for Janus *)
(* Compile by mosmlc -c Compiler.sml *)
(* Then recompile JC by mosmlc JC.sml -o JC *)

structure Compiler :> Compiler =
struct

  (* Use "raise Error (message,position)" for error messages *)
  exception Error of string*(int*int)

  (* Name generator.  Call with, e.g., t1 = "tmp"^newName () *)
  val counter = ref 0

  fun newName () = (counter := !counter + 1;
                  "_" ^ Int.toString (!counter)^ "_")

  (* Number to text with spim-compatible sign symbol *)
  fun makeConst n = if n>=0 then Int.toString n
                    else "-" ^ Int.toString (~n)

  (* link register *)
  val RA = "31"
  (* Register for use in calls *)
  val FP = "30"
  (* Register for stack pointer *)
  val SP = "29"

  (* Suggested register division *)
  val maxCaller = 9   (* highest caller-saves register *)
  val maxReg = 25     (* highest allocatable register *)
  (* this means Janus variables are primarily allocated in $10 - $25 *)
  (* and temporaries primarily in $2 ... $9 *)

  (* compile program *)
  fun compile (ins,outs,locals,stat,procs) =
    let
      val vars = findVars (ins@outs@locals)
      val readCode = makeReads ins
      val zeroCode = makeZero (outs@locals)
      val checkZeroCode = checkZero (ins@locals)
      val writeCode = makeWrites outs
      val procsCode = compileProcs procs vars
      val mainCode = compileStat stat vars
      val arrayCode = makeArraySpace (ins@outs@locals)
      val code = readCode @ zeroCode @ mainCode @ checkZeroCode @ writeCode
                 @ [Mips.J "_stop_"] @ procsCode
      val (code1, _, _)
             = RegAlloc.registerAlloc code [] 2 maxCaller maxReg
    in
      [Mips.TEXT "0x00400000",
       Mips.GLOBL "main",
       Mips.LABEL "main"]
      @ code1                     (* run program *)
      @ [Mips.LABEL "_stop_",
         Mips.LI ("2","10"),      (* syscall control = 10 *)
         Mips.SYSCALL,            (* exit *)
         Mips.LABEL "_NonZero_",  (* code for reporting nonzero variables *)
         Mips.LA ("4","_NonZeroString_"),
         Mips.LI ("2","4"), Mips.SYSCALL, (* print string *)
         Mips.J "_stop_",
         Mips.LABEL "_loopassertfail_",
         Mips.LA ("4","_LoopErrorString_"),
         Mips.LI ("2","4"), Mips.SYSCALL, (* print string *)
         Mips.J "_stop_",
         Mips.LABEL "_ifassertfail_",
         Mips.LA ("4","_IfErrorString_"),
         Mips.LI ("2","4"), Mips.SYSCALL, (* print string *)
         Mips.J "_stop_",
         Mips.DATA ""]
      @ arrayCode
      @ [Mips.LABEL "_cr_",       (* carriage return string *)
         Mips.ASCIIZ "\n",
         Mips.LABEL "_NonZeroString_",
         Mips.ASCIIZ "Nonzero non-output variable at end\n",
         Mips.LABEL "_IfErrorString_",
         Mips.ASCIIZ "Assertion at the end of an if-statement failed\n",
         Mips.LABEL "_LoopErrorString_",
         Mips.ASCIIZ "Assertion at the beginning of a loop-statement failed\n"

      ]
    end

  and makeArraySpace [] = []
    | makeArraySpace (Janus.IntVarDef _ :: defs) = makeArraySpace defs
    | makeArraySpace (Janus.ArrayVarDef (x,size,_) :: defs) =
        [Mips.LABEL ("_array_" ^ x),
         Mips.SPACE (Int.toString(size*4))]
        @ makeArraySpace defs

  (* find all Janus variable names *)
  and findVars [] = []
    | findVars (Janus.IntVarDef (x,_)::defs) =
        x::(findVars defs)
    | findVars (Janus.ArrayVarDef (x,_,_)::defs) =
        x::(findVars defs)

  (* read input variables *)
  and makeReads [] = []
    | makeReads (Janus.IntVarDef (x,_)::defs) =
        [Mips.LI ("2","5"), (* 5 = read_int syscall *)
         Mips.SYSCALL,
         Mips.MOVE (x,"2")]
        @ makeReads defs
    | makeReads (Janus.ArrayVarDef (x,size,_)::defs) =
      let
        val startlabel = "_readstart_" ^ newName()
        val arrayindex = "_index_" ^ newName()
        val countdown = "_counter_" ^ newName()
      in
        [Mips.LA (arrayindex, "_array_" ^ x),
         Mips.LI (countdown, Int.toString(size)),
         Mips.LABEL startlabel,
         
         Mips.LI ("2", "5"),
         Mips.SYSCALL,
         Mips.SW("2", arrayindex, "0"),

         Mips.ADDI(countdown, countdown, "-1"),
         Mips.ADDI(arrayindex, arrayindex, "4"),

         Mips.BNE("0", countdown, startlabel)] @
         makeReads defs
      end

     
  (* initialize variables to 0 *)
  and makeZero [] = []
    | makeZero (Janus.IntVarDef (x,_)::defs) =
        [Mips.MOVE (x,"0")]
        @ makeZero defs
    | makeZero (Janus.ArrayVarDef (x,size,_)::defs) =
      let
        val startlabel = "_makezerostart_" ^ newName()
        val arrayindex = "_index_" ^ newName()
        val countdown = "_counter_" ^ newName()
      in
        [Mips.LA (arrayindex, "_array_" ^ x),
         Mips.LI (countdown, Int.toString(size)),
         Mips.LABEL startlabel,
         
         Mips.SW("0", arrayindex, "0"),

         Mips.ADDI(countdown, countdown, "-1"),
         Mips.ADDI(arrayindex, arrayindex, "4"),

         Mips.BNE("0", countdown, startlabel)] @
         makeZero defs
      end
     

  (* check that variables are 0 *)
  and checkZero [] = []
    | checkZero (Janus.IntVarDef (x,_)::defs) =
        [Mips.BNE (x,"0","_NonZero_")]
        @ checkZero defs
    | checkZero (Janus.ArrayVarDef (x,size,_)::defs) =
      let
        val startlabel = "_checkzerostart_" ^ newName()
        val arrayindex = "_index_" ^ newName()
        val countdown = "_counter_" ^ newName()
        val tmpvar = "_zero_tmp_var_" ^ newName()
      in
        [Mips.LA (arrayindex, "_array_" ^ x),
         Mips.LI (countdown, Int.toString(size)),
         Mips.LABEL startlabel,
         
         Mips.LW(tmpvar, arrayindex, "0"),
         Mips.BNE(tmpvar, "0", "_NonZero_"),

         Mips.ADDI(countdown, countdown, "-1"),
         Mips.ADDI(arrayindex, arrayindex, "4"),

         Mips.BNE("0", countdown, startlabel)] @
         checkZero defs
      end
     

  (* write output variables *)
  and makeWrites [] = []
    | makeWrites (Janus.IntVarDef (x,_)::defs) =
        [Mips.MOVE ("4",x),
         Mips.LI ("2","1"), (* write_int syscall *)
         Mips.SYSCALL,
         Mips.LA ("4","_cr_"),
         Mips.LI ("2","4"), Mips.SYSCALL (* print CR *)]
    @ makeWrites defs
    | makeWrites (Janus.ArrayVarDef (x,size,_)::defs) =
      let
        val startlabel = "_writestart_" ^ newName()
        val arrayindex = "_index_" ^ newName()
        val countdown = "_counter_" ^ newName()
      in
        [Mips.LA (arrayindex, "_array_" ^ x),
         Mips.LI (countdown, Int.toString(size)),
         Mips.LABEL startlabel,
         
         Mips.LW("4", arrayindex, "0"),
         Mips.LI ("2", "1"),
         Mips.SYSCALL,
         Mips.LA ("4","_cr_"),
         Mips.LI ("2","4"), Mips.SYSCALL, (* print CR *)

         Mips.ADDI(countdown, countdown, "-1"),
         Mips.ADDI(arrayindex, arrayindex, "4"),

         Mips.BNE("0", countdown, startlabel)] @
         makeWrites defs
       end
     
  (* compile expression *)
  and compileExp e place =
    case e of
      Janus.Num (n,pos) =>
        if n<32768
        then [Mips.LI (place, makeConst n)]
        else
          [Mips.LUI (place, makeConst (n div 32768)),
           Mips.ORI (place, place, makeConst (n mod 32768))]
    | Janus.LVal (Janus.IntVar (x,p)) => [Mips.MOVE (place,x)]
    | Janus.LVal (Janus.ArrayIndex (x,e1,p)) =>
        let
          val addrName = "_addr_" ^ newName()
          val indexName = "_index_" ^ newName()
        in
          compileExp e1 indexName
          @ [Mips.SLL (indexName, indexName, "2"),
             Mips.LA  (addrName, "_array_" ^ x),
             Mips.ADD (addrName, addrName, indexName),
             Mips.LW  (place, addrName, "0")]
        end
    | Janus.Plus (e1,e2,pos) =>
        let
          val t1 = "_plus1_"^newName()
          val t2 = "_plus2_"^newName()
          val code1 = compileExp e1 t1
          val code2 = compileExp e2 t2
        in
          code1 @ code2 @ [Mips.ADD (place,t1,t2)]
    end
    | Janus.Minus (e1,e2,pos) =>
        let
          val t1 = "_minus1_"^newName()
          val t2 = "_minus2_"^newName()
          val code1 = compileExp e1 t1
          val code2 = compileExp e2 t2
        in
          code1 @ code2 @ [Mips.SUB (place,t1,t2)]
        end
    | Janus.Half (e1,pos) =>
        let
          val t1 = "_half_"^newName()
          val code1 = compileExp e1 t1
        in
          code1 @ [Mips.SRA (place,t1,"1")]
        end

  and compileCond c l1 l2 =
      let
        val exp1 = "_condexp1_" ^ newName()
        val exp2 = "_condexp2_" ^ newName()
      in
        case c of
          Janus.Equal(e1, e2, p) => compileExp e1 exp1 @
                              compileExp e2 exp2 @
                              [Mips.BEQ(exp1, exp2, l1),
                               Mips.J l2]
        | Janus.Less(e1, e2, p)  => compileExp e1 exp1 @
                              compileExp e2 exp2 @
                              [Mips.SLT(exp1, exp1, exp2),
                               Mips.BNE(exp1, "0", l1),
                               Mips.J l2]
        | Janus.Not(c1, p)     => compileCond c1 l2 l1 (* Switch-a-roo *)
        | Janus.And(c1, c2, p) =>
            let
              val middle = "_andmiddle_" ^ newName()
            in
              compileCond c1 middle l2 @
              Mips.LABEL middle ::
              compileCond c2 l1 l2
            end
        | Janus.Or(c1, c2, p)  =>
            let
              val middle = "_andmiddle_" ^ newName()
            in
              compileCond c1 l1 middle @
              Mips.LABEL middle ::
              compileCond c2 l1 l2
            end
      end

  (* compile statement *)
  and compileStat s vars =
    case s of
      Janus.Sequence (s1,s2,pos) =>
        compileStat s1 vars @ compileStat s2 vars
    | Janus.AddUpdate (Janus.IntVar (x,p),e1,pos) =>
        let
          val t1 = "_addupdate_"^newName()
          val code1 = compileExp e1 t1
        in
          code1 @ [Mips.ADD (x,x,t1)]
        end
    | Janus.AddUpdate (Janus.ArrayIndex (x,e1,p),e2,pos) =>
        let
          val result = "_addarrayupdate_"^newName()
          val addrName = "_addr_" ^ newName()
          val indexName = "_index_" ^ newName()
        in
          compileExp e2 result
          @ compileExp e1 indexName
          @ [Mips.SLL (indexName, indexName, "2"),
             Mips.LA  (addrName, "_array_" ^ x),
             Mips.ADD (addrName, addrName, indexName),
             Mips.LW  (indexName, addrName, "0"),
             Mips.ADD (indexName, indexName, result),
             Mips.SW  (indexName, addrName, "0")]
        end
    | Janus.SubUpdate (Janus.IntVar (x,p),e1,pos) =>
        let
          val t1 = "_subupdate_"^newName()
          val code1 = compileExp e1 t1
        in
          code1 @ [Mips.SUB (x,x,t1)]
        end
    | Janus.SubUpdate (Janus.ArrayIndex (x,e1,p),e2,pos) =>
        let
          val result = "_subarrayupdate_"^newName()
          val addrName = "_addr_" ^ newName()
          val indexName = "_index_" ^ newName()
        in
          compileExp e2 result
          @ compileExp e1 indexName
          @ [Mips.SLL (indexName, indexName, "2"),
             Mips.LA  (addrName, "_array_" ^ x),
             Mips.ADD (addrName, addrName, indexName),
             Mips.LW  (indexName, addrName, "0"),
             Mips.SUB (indexName, indexName, result),
             Mips.SW  (indexName, addrName, "0")]
        end
    | Janus.Skip p => []
    | Janus.If (c1,s1,s2,c2,(line,col)) =>
        let
          val endvar = "_ifend_" ^ newName()
          val cond1  = "_if1_" ^ newName()
          val cond2  = "_if2_" ^ newName()
        in
          compileCond c1 cond1 cond2 @
          Mips.LABEL cond1 ::
          compileStat s1 vars @
          compileCond c2 endvar "_ifassertfail_" @
          Mips.LABEL cond2 ::
          compileStat s2 vars @
          compileCond c2 "_ifassertfail_" endvar @
          [Mips.LABEL endvar]
        end
    | Janus.Loop (c1,s1,s2,c2,(line,col)) =>
        let
          val beginvar = "_loopbegin" ^ newName()
          val endvar = "_loopend_" ^ newName()
          val cond1  = "_loop1_" ^ newName()
          val cond2  = "_loop2_" ^ newName()
        in
          Mips.LABEL beginvar ::
          compileCond c1 cond1 "_loopassertfail_" @
          Mips.LABEL cond1 ::
          compileStat s1 vars @
          compileCond c2 endvar cond2 @
          Mips.LABEL cond2 ::
          compileStat s2 vars @
          compileCond c1 "_loopassertfail_" cond1 @
          [Mips.LABEL endvar]
        end
    | Janus.Call (proc,pos) =>
        [Mips.ADDI (SP,SP,"-4"), (* push old return address *)
         Mips.SW (RA,SP,"0"),
         Mips.JAL ("_forward_"^proc,vars)] (* and JAL to procedure *)
    (* all global variables are preserved *)
    | Janus.Uncall (proc,pos) =>
        [Mips.ADDI (SP,SP,"-4"), (* push old return address *)
         Mips.SW (RA,SP,"0"),
         Mips.JAL ("_backward_"^proc,vars)] (* and JAL to procedure *)

   and reverseStatement stat =
      case stat of
        Janus.Sequence (s1,s2,p)  => Janus.Sequence(reverseStatement s2, reverseStatement s1,p)
      | Janus.AddUpdate (l,e,p)   => Janus.SubUpdate(l,e,p)
      | Janus.SubUpdate (l,e,p)   => Janus.AddUpdate(l,e,p)
      | Janus.If(c1,s1,s2,c2,p)   => Janus.If(c2,reverseStatement s1,reverseStatement s2,c1,p)
      | Janus.Loop(c1,s1,s2,c2,p) => Janus.Loop(c2,reverseStatement s1,reverseStatement s2,c1,p)
      | Janus.Skip p              => Janus.Skip p
      | Janus.Call (x,p)          => Janus.Uncall(x,p)
      | Janus.Uncall (x,p)        => Janus.Call(x,p)

  and compileProcs [] vars = []
    | compileProcs ((pname,body,pos)::procs) vars =
        [Mips.LABEL ("_forward_"^pname)]
        @ compileStat body vars
        @ [Mips.MOVE (FP,RA), (* move return address to FP *)
           Mips.LW (RA,SP,"0"), (* restore old return address *)
           Mips.ADDI (SP,SP,"4"),
           Mips.JR (FP,vars)] (* and jump back *)
        (* all global variables are preserved *)

        @ compileProcs procs vars

        @ [Mips.LABEL ("_backward_"^pname)]
        @ compileStat (reverseStatement body) vars
        @ [Mips.MOVE (FP,RA), (* move return address to FP *)
           Mips.LW (RA,SP,"0"), (* restore old return address *)
           Mips.ADDI (SP,SP,"4"),
           Mips.JR (FP,vars)] (* and jump back *)
        (* all global variables are preserved *)
end
