signature Type =
sig

  exception Error of string*(int*int)

  val checkProgram : Janus.Prog -> unit

end
