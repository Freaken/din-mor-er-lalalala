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
         Mips.DATA "",
	     Mips.LABEL "_cr_",       (* carriage return string *)
	     Mips.ASCIIZ "\n",
	     Mips.LABEL "_NonZeroString_",
	     Mips.ASCIIZ "Nonzero non-output variable at end\n"]
         (* CODE AND STRINGS FOR REPORTING ASSERTION FAILURES SHOULD BE ADDED *)
    end

  (* find all Janus variable names *)
  and findVars [] = []
    | findVars (Janus.IntVarDef (x,_)::defs) =
        x::(findVars defs)
    | findVars (Janus.ArrayVarDef (x,_,_)::defs) =
       (* TO BE ADDED *) findVars defs

  (* read input variables *)
  and makeReads [] = []
    | makeReads (Janus.IntVarDef (x,_)::defs) =
        [Mips.LI ("2","5"), (* 5 = read_int syscall *)
	     Mips.SYSCALL,
     	 Mips.MOVE (x,"2")]
	@ makeReads defs
    | makeReads (Janus.ArrayVarDef (x,size,_)::defs) =
        [] (* TO BE ADDED *)
	@ makeReads defs
	 
  (* initialize variables to 0 *)
  and makeZero [] = []
    | makeZero (Janus.IntVarDef (x,_)::defs) =
        [Mips.MOVE (x,"0")]
	@ makeZero defs
    | makeZero (Janus.ArrayVarDef (x,size,_)::defs) =
        [] (* TO BE ADDED *)
	@ makeZero defs
	 

  (* check that variables are 0 *)
  and checkZero [] = []
    | checkZero (Janus.IntVarDef (x,_)::defs) =
        [Mips.BNE (x,"0","_NonZero_")]
	@ checkZero defs
    | checkZero (Janus.ArrayVarDef (x,size,_)::defs) =
        []  (* TO BE ADDED *)
	@ checkZero defs
	 

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
        []  (* TO BE ADDED *)
	@ makeWrites defs
	 
  (* compile expression *)
  and compileExp e place =
    case e of
      Janus.Num (n,pos) =>
        if n<32768 then
	  [Mips.LI (place, makeConst n)]
	else
	  [Mips.LUI (place, makeConst (n div 32768)),
	   Mips.ORI (place, place, makeConst (n mod 32768))]
    | Janus.LVal (Janus.IntVar (x,p)) => [Mips.MOVE (place,x)]
    | Janus.LVal (Janus.ArrayIndex (x,e1,p)) =>
        [] (* TO BE ADDED *)
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
        [] (* TO BE ADDED *)
    | Janus.SubUpdate (Janus.IntVar (x,p),e1,pos) =>
        let
	  val t1 = "_subupdate_"^newName()
	  val code1 = compileExp e1 t1
	in
	  code1 @ [Mips.SUB (x,x,t1)]
	end
    | Janus.SubUpdate (Janus.ArrayIndex (x,e1,p),e2,pos) =>
        [] (* TO BE ADDED *)
    | Janus.Skip p => []
    | Janus.If (c1,s1,s2,c2,(line,col)) =>
        [] (* TO BE ADDED *)
    | Janus.Loop (c1,s1,s2,c2,(line,col)) =>
        [] (* TO BE ADDED *)
    | Janus.Call (proc,pos) =>
        [Mips.ADDI (SP,SP,"-4"), (* push old return address *)
	     Mips.SW (RA,SP,"0"),
	     Mips.JAL ("_forward_"^proc,vars)] (* and JAL to procedure *)
	(* all global variables are preserved *)
    | Janus.Uncall (proc,pos) =>
        [] (* TO BE ADDED *)



  and compileProcs [] vars = []
    | compileProcs ((pname,body,pos)::procs) vars =
        [Mips.LABEL ("_forward_"^pname)] @
	compileStat body vars @
	[Mips.MOVE (FP,RA), (* move return address to FP *)
	 Mips.LW (RA,SP,"0"), (* restore old return address *)
	 Mips.ADDI (SP,SP,"4"),
	 Mips.JR (FP,vars)] (* and jump back *)
	(* all global variables are preserved *)
	@ compileProcs procs vars
	(* NEEDS MODIFICATION *)

end
