package dev.argon.expr

sealed trait BuiltinBase derives CanEqual
sealed trait BuiltinType extends BuiltinBase derives CanEqual

enum NullaryBuiltin extends BuiltinBase derives CanEqual {
  case IntType extends NullaryBuiltin with BuiltinType
  case BoolType extends NullaryBuiltin with BuiltinType
  case StringType extends NullaryBuiltin with BuiltinType
  case NeverType extends NullaryBuiltin with BuiltinType
}

enum UnaryBuiltin extends BuiltinBase derives CanEqual {
  case IntNegate
  case IntBitNot
}

enum BinaryBuiltin extends BuiltinBase derives CanEqual {
  case ConjunctionType extends BinaryBuiltin with BuiltinType
  case DisjunctionType extends BinaryBuiltin with BuiltinType

  case IntAdd
  case IntSub
  case IntMul
  case IntBitAnd
  case IntBitOr
  case IntBitXOr
  case IntBitShiftLeft
  case IntBitShiftRight
  case IntEQ
  case IntNE
  case IntLT
  case IntLE
  case IntGT
  case IntGE


  case StringConcat
  case StringEQ
  case StringNE
}

