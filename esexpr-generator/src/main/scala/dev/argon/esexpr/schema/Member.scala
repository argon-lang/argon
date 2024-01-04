package dev.argon.esexpr.schema

import dev.argon.esexpr.*

enum Member derives ESExprCodec, CanEqual {
  val name: String

  case Field(name: String, fieldType: DataType)
  case Method(
    name: String,
    @keyword `override`: Boolean = false,
    @keyword async: Boolean = true,
    params: Seq[MethodParameter],
    returnType: DataType
  )
}