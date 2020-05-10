package dev.argon.compiler.expr

sealed trait PrimitiveOperation
object PrimitiveOperation {
  case object AddInt extends PrimitiveOperation
  case object SubInt extends PrimitiveOperation
  case object MulInt extends PrimitiveOperation
  case object IntEqual extends PrimitiveOperation
}
