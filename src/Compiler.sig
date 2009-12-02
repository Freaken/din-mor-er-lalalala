signature Compiler =
sig

  exception Error of string*(int*int)

  val compile : Janus.Prog -> Mips.mips list

end
