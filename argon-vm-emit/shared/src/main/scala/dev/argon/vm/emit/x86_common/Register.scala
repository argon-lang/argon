package dev.argon.vm.emit.x86_common

final case class Register[+Size <: RegisterSize, +Name <: RegisterName[Size]](
  name: Name,
  size: Size,
)

object Register {

  given[Size <: RegisterSize, Name <: RegisterName[Size]]: CanEqual[Register[Size, Name], Register[Size, Name]] =
    CanEqual.derived

}

sealed trait RegisterName[-Size <: RegisterSize] derives CanEqual
object RegisterName {
  sealed trait X86_64[-Size <: RegisterSize] extends RegisterName[Size] derives CanEqual
  sealed trait X86_32[-Size <: RegisterSize] extends X86_64[Size] derives CanEqual
  sealed trait X86_16[-Size <: RegisterSize] extends X86_32[Size] derives CanEqual

  case object AX extends X86_16[RegisterSize.X86_64 | RegisterSize.HighHalf] derives CanEqual
  case object BX extends X86_16[RegisterSize.X86_64 | RegisterSize.HighHalf] derives CanEqual
  case object CX extends X86_16[RegisterSize.X86_64 | RegisterSize.HighHalf] derives CanEqual
  case object DX extends X86_16[RegisterSize.X86_64 | RegisterSize.HighHalf] derives CanEqual
  case object SP extends X86_16[RegisterSize.X86_64] derives CanEqual
  case object SI extends X86_16[RegisterSize.X86_64] derives CanEqual
  case object DI extends X86_16[RegisterSize.X86_64] derives CanEqual
  case object R8 extends X86_64[RegisterSize.X86_64] derives CanEqual
  case object R9 extends X86_64[RegisterSize.X86_64] derives CanEqual
  case object R10 extends X86_64[RegisterSize.X86_64] derives CanEqual
  case object R11 extends X86_64[RegisterSize.X86_64] derives CanEqual
  case object R12 extends X86_64[RegisterSize.X86_64] derives CanEqual
  case object R13 extends X86_64[RegisterSize.X86_64] derives CanEqual
  case object R14 extends X86_64[RegisterSize.X86_64] derives CanEqual
  case object R15 extends X86_64[RegisterSize.X86_64] derives CanEqual
}

sealed trait RegisterSize derives CanEqual {
  type Literal
}
object RegisterSize {
  type HighHalf = HighHalf.type
  sealed trait X86_64 extends RegisterSize derives CanEqual
  sealed trait X86_32 extends X86_64 derives CanEqual
  sealed trait X86_16 extends X86_32 derives CanEqual

  case object HighHalf extends RegisterSize derives CanEqual {
    override type Literal = Byte
  }

  case object LowHalf extends X86_16 derives CanEqual {
    override type Literal = Byte
  }

  case object Word extends X86_16 derives CanEqual {
    override type Literal = Short
  }

  case object DWord extends X86_32 derives CanEqual {
    override type Literal = Int
  }

  case object QWord extends X86_64 derives CanEqual {
    override type Literal = Long
  }

}

