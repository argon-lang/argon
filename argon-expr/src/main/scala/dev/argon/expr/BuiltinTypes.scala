package dev.argon.expr

sealed trait BuiltinType derives CanEqual

enum NullaryBuiltin derives CanEqual {
  case IntType extends NullaryBuiltin with BuiltinType
  case BoolType extends NullaryBuiltin with BuiltinType
  case StringType extends NullaryBuiltin with BuiltinType
  case NeverType extends NullaryBuiltin with BuiltinType
}

enum UnaryBuiltin derives CanEqual {
  case IntNegate
}

enum BinaryBuiltin derives CanEqual {
  case ConjunctionType extends BinaryBuiltin with BuiltinType
  case DisjunctionType extends BinaryBuiltin with BuiltinType

  case IntAdd
  case IntSub
  case IntMul
  case IntEQ
  case IntNE
  case IntLT
  case IntLE
  case IntGT
  case IntGE

  case StringConcat
}

