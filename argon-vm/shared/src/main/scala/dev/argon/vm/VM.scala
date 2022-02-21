package dev.argon.vm

trait VM {

  type NativeLiteral
  def constINative(out: LocalId, i: NativeLiteral): Instruction
  def convertSNative(out: LocalId, in: LocalId, overflow: Boolean): Instruction
  def convertU_SNative(out: LocalId, in: LocalId, overflow: Boolean): Instruction
  def convertUNative(out: LocalId, in: LocalId, overflow: Boolean): Instruction
  def convertU_UNative(out: LocalId, in: LocalId, overflow: Boolean): Instruction


  type ExternFunctionBody

  final case class FunctionBody(
    locals: Map[LocalId, VMType],
    parameters: Seq[LocalId],
    block: InstructionBlock,
  )

  final case class VMFunction(
    paramTypes: Seq[VMType],
    returnType: VMType,
    body: Option[FunctionBody | ExternFunctionBody],
  )


  final case class VMProgram(
    functions: Map[FunctionId, VMFunction],
  )

}


