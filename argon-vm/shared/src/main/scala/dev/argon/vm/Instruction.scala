package dev.argon.vm

enum Instruction {
  // Constants
  case ConstI8(out: LocalId, b: Byte)
  case ConstI16(out: LocalId, s: Short)
  case ConstI32(out: LocalId, i: Int)
  case ConstI64(out: LocalId, l: Long)
  case ConstF32(out: LocalId, f: Float)
  case ConstF64(out: LocalId, d: Double)
  case ConstInt(out: LocalId, i: BigInt)
  case ConstString(out: LocalId, s: String)
  case ConstNull(out: LocalId)

  // Conversions
  case ConvertS8(out: LocalId, in: LocalId, overflow: OverflowMode)
  case ConvertU8(out: LocalId, in: LocalId, overflow: OverflowMode)
  
  case ConvertS16(out: LocalId, in: LocalId, overflow: Boolean)
  case ConvertU_S16(out: LocalId, in: LocalId, overflow: Boolean)
  case ConvertU16(out: LocalId, in: LocalId, overflow: Boolean)
  case ConvertU_U16(out: LocalId, in: LocalId, overflow: Boolean)
  
  case ConvertS32(out: LocalId, in: LocalId, overflow: Boolean)
  case ConvertU_S32(out: LocalId, in: LocalId, overflow: Boolean)
  case ConvertU32(out: LocalId, in: LocalId, overflow: Boolean)
  case ConvertU_U32(out: LocalId, in: LocalId, overflow: Boolean)

  case ConvertS64(out: LocalId, in: LocalId, overflow: Boolean)
  case ConvertU_S64(out: LocalId, in: LocalId, overflow: Boolean)
  case ConvertU64(out: LocalId, in: LocalId, overflow: Boolean)
  case ConvertU_U64(out: LocalId, in: LocalId, overflow: Boolean)
  
  case ConvertF32(out: LocalId, in: LocalId)
  case ConvertU_F32(out: LocalId, in: LocalId)
  case ConvertF64(out: LocalId, in: LocalId)
  case ConvertU_F64(out: LocalId, in: LocalId)

  // Common Operations
  case Add(out: LocalId, a: LocalId, b: LocalId, overflow: OverflowMode)
  case Sub(out: LocalId, a: LocalId, b: LocalId, overflow: OverflowMode)
  case Mul(out: LocalId, a: LocalId, b: LocalId, overflow: OverflowMode)
  case DivS(out: LocalId, a: LocalId, b: LocalId, overflow: Boolean)
  case DivU(out: LocalId, a: LocalId, b: LocalId)
  case RemS(out: LocalId, a: LocalId, b: LocalId)
  case RemU(out: LocalId, a: LocalId, b: LocalId)
  case Negate(out: LocalId, a: LocalId, overflow: Boolean)
  case ShiftLeft(out: LocalId, a: LocalId, b: LocalId)
  case ShiftRightS(out: LocalId, a: LocalId, b: LocalId)
  case ShiftRightU(out: LocalId, a: LocalId, b: LocalId)
  case BitAnd(out: LocalId, a: LocalId, b: LocalId)
  case BitOr(out: LocalId, a: LocalId, b: LocalId)
  case BitXOr(out: LocalId, a: LocalId, b: LocalId)
  case BitNot(out: LocalId, a: LocalId)

  // Comparisons
  case EqualTo(out: LocalId, a: LocalId, b: LocalId)
  case NotEqualTo(out: LocalId, a: LocalId, b: LocalId)
  case LessThan(out: LocalId, a: LocalId, b: LocalId)
  case LessThanU(out: LocalId, a: LocalId, b: LocalId)
  case LessThanEq(out: LocalId, a: LocalId, b: LocalId)
  case LessThanEqU(out: LocalId, a: LocalId, b: LocalId)
  case GreaterThan(out: LocalId, a: LocalId, b: LocalId)
  case GreaterThanU(out: LocalId, a: LocalId, b: LocalId)
  case GreaterThanEq(out: LocalId, a: LocalId, b: LocalId)
  case GreaterThanEqU(out: LocalId, a: LocalId, b: LocalId)
  case EqualToZero(out: LocalId, a: LocalId)
  case NotEqualToZero(out: LocalId, a: LocalId)

  // Functions
  case Call(id: FunctionId, args: Seq[LocalId], result: LocalId, virtual: Boolean)

  // Memory
  case DerefLoad(out: LocalId, ptr: LocalId, elementType: VMType, volatile: Boolean)
  case DerefStore(ptr: LocalId, src: LocalId, elementType: VMType, volatile: Boolean)

  // Objects
  case CreateObject(out: LocalId, objClass: ClassId)
  case CreateArray(out: LocalId, objClass: ClassId, length: LocalId)

  case LoadField(out: LocalId, instance: LocalId, field: FieldId, volatile: Boolean)
  case StoreField(instance: LocalId, value: LocalId, field: FieldId, volatile: Boolean)
  case LoadFieldReference(out: LocalId, instance: LocalId, field: FieldId)

  case ArrayLength(out: LocalId, instance: LocalId, cls: ClassId)
  case LoadArrayElement(out: LocalId, instance: LocalId, index: LocalId, cls: ClassId, volatile: Boolean)
  case StoreArrayElement(instance: LocalId, value: LocalId, index: LocalId, cls: ClassId, volatile: Boolean)
  case LoadArrayElementReference(out: LocalId, instance: LocalId, index: LocalId, cls: ClassId)
}

enum OverflowMode derives CanEqual {
  case None, Signed, Unsigned
}

enum JumpInstruction {
  case TailCall(id: FunctionId, args: Seq[LocalId], virtual: Boolean)
  case Return(value: LocalId)
  case Jump(label: LabelId)
  case JumpZero(value: LocalId, zTarget: LabelId, nzTarget: LabelId)
  case Throw(exception: LocalId)
}

final case class ControlFlowGraph(blocks: Map[LabelId, BasicBlock], start: LabelId)

sealed trait BasicBlock

final case class InstructionBlock(instructions: Seq[Instruction], jump: JumpInstruction) extends BasicBlock
final case class CatchBlock(body: ControlFlowGraph, exceptionVariable: LocalId, handler: LabelId) extends BasicBlock
final case class WithLocalReference(refHolder: LocalId, referenced: LocalId, block: ControlFlowGraph) extends BasicBlock
