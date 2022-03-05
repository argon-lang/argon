package dev.argon.vm.interpreter

import dev.argon.vm.*
import zio.*

object InterpreterVM extends VM {

  override type NativeLiteral = Long
  override def constINative(out: LocalId, i: Long): Instruction = Instruction.ConstI64(out, i)
  override def convertSNative(out: LocalId, in: LocalId, overflow: Boolean): Instruction = Instruction.ConvertS64(out, in, overflow)
  override def convertU_SNative(out: LocalId, in: LocalId, overflow: Boolean): Instruction = Instruction.ConvertU_S64(out, in, overflow)
  override def convertUNative(out: LocalId, in: LocalId, overflow: Boolean): Instruction = Instruction.ConvertU64(out, in, overflow)
  override def convertU_UNative(out: LocalId, in: LocalId, overflow: Boolean): Instruction = Instruction.ConvertU_U64(out, in, overflow)

  type ExternFunctionBody = (Seq[VMValue]) => IO[GCObject | Throwable, VMValue]

}