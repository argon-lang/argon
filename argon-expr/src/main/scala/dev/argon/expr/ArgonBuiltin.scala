package dev.argon.expr

sealed trait ArgonBuiltin[N <: Int] derives CanEqual
object ArgonBuiltin {
  sealed trait Type[N <: Int] extends ArgonBuiltin[N]
  sealed trait SimpleValue extends ArgonBuiltin[0]
  sealed trait UnaryOperation extends ArgonBuiltin[1]
  sealed trait BinaryOperation extends ArgonBuiltin[2]

  case object IntType extends SimpleValue with Type[0]
  case object IntAdd extends BinaryOperation
  case object IntSub extends BinaryOperation
  case object IntMul extends BinaryOperation
  case object IntEQ extends BinaryOperation
  case object IntNE extends BinaryOperation
  case object IntLT extends BinaryOperation
  case object IntLE extends BinaryOperation
  case object IntGT extends BinaryOperation
  case object IntGE extends BinaryOperation
  case object IntNegate extends UnaryOperation

  case object BoolType extends SimpleValue with Type[0]

  case object StringType extends SimpleValue with Type[0]
  case object StringConcat extends BinaryOperation


  case object NeverType extends SimpleValue with Type[0]

}

