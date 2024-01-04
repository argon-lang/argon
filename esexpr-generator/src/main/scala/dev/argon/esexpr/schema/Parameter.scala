package dev.argon.esexpr.schema

import dev.argon.esexpr.*

enum Parameter derives ESExprCodec, CanEqual {
  val name: String
  
  @constructor("kwarg")
  case KeywordParam(name: String, paramType: DataType, @keyword optional: Boolean = false, @keyword("default-value") defaultValue: Option[ESExpr])

  @constructor("dict")
  case Dict(name: String, paramType: DataType)

  @constructor("arg")
  case PositionalParam(name: String, paramType: DataType)

  @constructor("vararg")
  case VarArgParam(name: String, paramType: DataType)
}
