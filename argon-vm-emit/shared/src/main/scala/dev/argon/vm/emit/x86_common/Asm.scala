package dev.argon.vm.emit.x86_common



sealed trait AssemblyInstruction[RSize <: RegisterSize, RName <: RegisterName[RSize]]
object AssemblyInstruction {
  sealed trait X86_64[RSize <: RegisterSize, RName <: RegisterName[RSize]] extends AssemblyInstruction[RSize, RName]
  sealed trait X86_32[RSize <: RegisterSize, RName <: RegisterName[RSize]] extends X86_64[RSize, RName]
  sealed trait X86_16[RSize <: RegisterSize, RName <: RegisterName[RSize]] extends X86_32[RSize, RName]

  sealed trait X86_16Insn[RSize <: RegisterSize, RName <: RegisterName[RSize]] extends X86_16[RSize, RName]

  final class Mov[RSize <: RegisterSize, RName <: RegisterName[RSize], S <: RSize](dest: Register[S, RName], src: Register[S, RName]) extends X86_16Insn[RSize, RName]
  final class MovConst[RSize <: RegisterSize, RName <: RegisterName[RSize], S <: RSize](dest: Register[S, RName], value: dest.size.Literal) extends X86_16Insn[RSize, RName]


  

}



