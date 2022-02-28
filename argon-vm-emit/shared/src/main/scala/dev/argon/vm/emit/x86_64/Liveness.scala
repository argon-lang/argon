package dev.argon.vm.emit.x86_64

import dev.argon.vm.*
import dev.argon.vm.emit.common
import dev.argon.vm.emit.x86_common.{RegisterName, RegisterSize}

object Liveness extends common.Liveness {

  override type StorageLocation = RegisterName[RegisterSize.X86_64]

  private val anywhere: Set[StorageLocation] = Set(
    RegisterName.AX,
    RegisterName.BX,
    RegisterName.CX,
    RegisterName.DX,
    RegisterName.SI,
    RegisterName.DI,
    RegisterName.R8,
    RegisterName.R9,
    RegisterName.R10,
    RegisterName.R11,
    RegisterName.R12,
    RegisterName.R13,
    RegisterName.R14,
    RegisterName.R15,
  )

  def instructionKill(insn: Instruction): Map[LocalId, Set[StorageLocation]] =
    import Instruction.*
    insn match {
      case ConstI8(out, _) => Map(out -> anywhere)
      case ConstI16(out, _) => Map(out -> anywhere)
      case ConstI32(out, _) => Map(out -> anywhere)
      case ConstI64(out, _) => Map(out -> anywhere)
      case ConstF32(out, _) => Map(out -> anywhere)
      case ConstF64(out, _) => Map(out -> anywhere)
      case ConstInt(out, _) => Map(out -> anywhere)
      case ConstString(out, _) => Map(out -> anywhere)
      case ConstNull(out) => Map(out -> anywhere)

      case ConvertS8(out, _, _) => Map(out -> anywhere)
      case ConvertU8(out, _, _) => Map(out -> anywhere)

      case ConvertS16(out, _, _) => Map(out -> anywhere)
      case ConvertU_S16(out, _, _) => Map(out -> anywhere)
      case ConvertU16(out, _, _) => Map(out -> anywhere)
      case ConvertU_U16(out, _, _) => Map(out -> anywhere)

      case ConvertS32(out, _, _) => Map(out -> anywhere)
      case ConvertU_S32(out, _, _) => Map(out -> anywhere)
      case ConvertU32(out, _, _) => Map(out -> anywhere)
      case ConvertU_U32(out, _, _) => Map(out -> anywhere)

      case ConvertS64(out, _, _) => Map(out -> anywhere)
      case ConvertU_S64(out, _, _) => Map(out -> anywhere)
      case ConvertU64(out, _, _) => Map(out -> anywhere)
      case ConvertU_U64(out, _, _) => Map(out -> anywhere)

      case ConvertF32(out, _) => Map(out -> anywhere)
      case ConvertU_F32(out, _) => Map(out -> anywhere)
      case ConvertF64(out, _) => Map(out -> anywhere)
      case ConvertU_F64(out, _) => Map(out -> anywhere)

      case Add(out, _, _, _) => Map(out -> anywhere)
      case Sub(out, _, _, _) => Map(out -> anywhere)
      case Mul(out, _, _, _) => Map(out -> anywhere)
      case DivS(out, _, _, _) => Map(out -> anywhere)
      case DivU(out, _, _) => Map(out -> anywhere)
      case RemS(out, _, _) => Map(out -> anywhere)
      case RemU(out, _, _) => Map(out -> anywhere)
      case Negate(out, _, _) => Map(out -> anywhere)
      case ShiftLeft(out, _, _) => Map(out -> anywhere)
      case ShiftRightS(out, _, _) => Map(out -> anywhere)
      case ShiftRightU(out, _, _) => Map(out -> anywhere)
      case BitAnd(out, _, _) => Map(out -> anywhere)
      case BitOr(out, _, _) => Map(out -> anywhere)
      case BitXOr(out, _, _) => Map(out -> anywhere)
      case BitNot(out, _) => Map(out -> anywhere)
      case EqualTo(out, _, _) => Map(out -> anywhere)
      case NotEqualTo(out, _, _) => Map(out -> anywhere)
      case LessThan(out, _, _) => Map(out -> anywhere)
      case LessThanU(out, _, _) => Map(out -> anywhere)
      case LessThanEq(out, _, _) => Map(out -> anywhere)
      case LessThanEqU(out, _, _) => Map(out -> anywhere)
      case GreaterThan(out, _, _) => Map(out -> anywhere)
      case GreaterThanU(out, _, _) => Map(out -> anywhere)
      case GreaterThanEq(out, _, _) => Map(out -> anywhere)
      case GreaterThanEqU(out, _, _) => Map(out -> anywhere)
      case EqualToZero(out, _) => Map(out -> anywhere)
      case NotEqualToZero(out, _) => Map(out -> anywhere)


      case Call(_, _, result, _) => Map(result -> anywhere)

      // Memory
      case DerefLoad(out, _, _, _) => Map(out -> anywhere)
      case DerefStore(_, _, _, _) => Map.empty

      // Objects
      case CreateObject(out, _) => Map(out -> anywhere)
      case CreateArray(out, _, _) => Map(out -> anywhere)

      case LoadField(out, _, _, _) => Map(out -> anywhere)
      case StoreField(_, _, _, _) => Map.empty
      case LoadFieldReference(out, _, _) => Map(out -> anywhere)

      case ArrayLength(out, _, _) => Map(out -> anywhere)
      case LoadArrayElement(out, _, _, _, _) => Map(out -> anywhere)
      case StoreArrayElement(_, _, _, _, _) => Map.empty
      case LoadArrayElementReference(out, _, _, _) => Map(out -> anywhere)
    }
  end instructionKill

  def instructionGen(insn: Instruction): Map[LocalId, Set[StorageLocation]] =
    import Instruction.*
    insn match {
      case ConstI8(_, _) => Map.empty
      case ConstI16(_, _) => Map.empty
      case ConstI32(_, _) => Map.empty
      case ConstI64(_, _) => Map.empty
      case ConstF32(_, _) => Map.empty
      case ConstF64(_, _) => Map.empty
      case ConstInt(_, _) => Map.empty
      case ConstString(_, _) => Map.empty
      case ConstNull(_) => Map.empty

      case ConvertS8(_, in, _) => Map(in -> anywhere)
      case ConvertU8(_, in, _) => Map(in -> anywhere)

      case ConvertS16(_, in, _) => Map(in -> anywhere)
      case ConvertU_S16(_, in, _) => Map(in -> anywhere)
      case ConvertU16(_, in, _) => Map(in -> anywhere)
      case ConvertU_U16(_, in, _) => Map(in -> anywhere)

      case ConvertS32(_, in, _) => Map(in -> anywhere)
      case ConvertU_S32(_, in, _) => Map(in -> anywhere)
      case ConvertU32(_, in, _) => Map(in -> anywhere)
      case ConvertU_U32(_, in, _) => Map(in -> anywhere)

      case ConvertS64(_, in, _) => Map(in -> anywhere)
      case ConvertU_S64(_, in, _) => Map(in -> anywhere)
      case ConvertU64(_, in, _) => Map(in -> anywhere)
      case ConvertU_U64(_, in, _) => Map(in -> anywhere)

      case ConvertF32(_, in) => Map(in -> anywhere)
      case ConvertU_F32(_, in) => Map(in -> anywhere)
      case ConvertF64(_, in) => Map(in -> anywhere)
      case ConvertU_F64(_, in) => Map(in -> anywhere)

      case Add(_, a, b, _) => Map(a -> anywhere, b -> anywhere)
      case Sub(_, a, b, _) => Map(a -> anywhere, b -> anywhere)
      case Mul(_, a, b, _) => Map(a -> anywhere, b -> anywhere)
      case DivS(_, a, b, _) => Map(a -> anywhere, b -> anywhere)
      case DivU(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case RemS(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case RemU(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case Negate(_, a, _) => Map(a -> anywhere)
      case ShiftLeft(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case ShiftRightS(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case ShiftRightU(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case BitAnd(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case BitOr(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case BitXOr(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case BitNot(_, a) => Map(a -> anywhere)

      // Comparisons
      case EqualTo(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case NotEqualTo(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case LessThan(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case LessThanU(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case LessThanEq(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case LessThanEqU(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case GreaterThan(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case GreaterThanU(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case GreaterThanEq(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case GreaterThanEqU(_, a, b) => Map(a -> anywhere, b -> anywhere)
      case EqualToZero(_, a) => Map(a -> anywhere)
      case NotEqualToZero(_, a) => Map(a -> anywhere)

      // Functions
      case Call(_, args, _, _) => ???

      // Memory
      case DerefLoad(_, ptr, _, _) => Map(ptr -> anywhere)
      case DerefStore(ptr, src, _, _) => Map(ptr -> anywhere, src -> anywhere)

      // Objects
      case CreateObject(_, _) => Map.empty
      case CreateArray(_, _, length) => Map(length -> anywhere)

      case LoadField(_, instance, _, _) => Map(instance -> anywhere)
      case StoreField(instance, value, _, _) => Map(instance -> anywhere, value -> anywhere)
      case LoadFieldReference(_, instance, _) => Map(instance -> anywhere)

      case ArrayLength(_, instance, _) => Map(instance -> anywhere)
      case LoadArrayElement(_, instance, index, _, _) => Map(instance -> anywhere, index -> anywhere)
      case StoreArrayElement(instance, value, index, _, _) => Map(instance -> anywhere, value -> anywhere, index -> anywhere)
      case LoadArrayElementReference(_, instance, index, _) => Map(instance -> anywhere, index -> anywhere)
    }
  end instructionGen

  def jumpInstructionGen(jump: JumpInstruction): Map[LocalId, Set[StorageLocation]] =
    jump match {
      case JumpInstruction.TailCall(_, args, _) => ???
      case JumpInstruction.Return(value) => ???
      case JumpInstruction.Jump(_) => Map.empty
      case JumpInstruction.JumpZero(value, _, _) => Map(value -> anywhere)
      case JumpInstruction.Throw(ex) => Map(ex -> anywhere)
    }
}
