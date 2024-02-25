package dev.argon.expr

enum NullaryBuiltin derives CanEqual {
  case IntType
  case BoolType
  case StringType
  case NeverType
}

enum UnaryBuiltin derives CanEqual {
  case IntNegate
}

enum BinaryBuiltin derives CanEqual {
  case ConjunctionType
  case DisjunctionType

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

