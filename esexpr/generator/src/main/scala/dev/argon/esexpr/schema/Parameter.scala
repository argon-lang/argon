package dev.argon.esexpr.schema

import dev.argon.esexpr.*

enum Parameter derives ESExprCodec {
  @constructor("kwarg")
  case KeywordParam(name: String, @keyword("lang-name") langName: Option[String], paramType: DataType, @keyword optional: Boolean = false, @keyword("default-value") defaultValue: Option[ESExpr])

  @constructor("arg")
  case PositionalParam(@keyword("lang-name") langName: String, paramType: DataType)

  @constructor("vararg")
  case VarArgParam(@keyword("lang-name") langName: String, paramType: DataType)
}
