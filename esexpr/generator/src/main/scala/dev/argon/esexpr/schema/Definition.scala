package dev.argon.esexpr.schema

import dev.argon.esexpr.*

enum Definition derives ESExprCodec {
  @inlineValue
  case ConstructorDef(constructor: Constructor)
  case Enum(name: String, @keyword("lang-name") langName: Option[String], cases: Constructor*)
  case Const(@keyword("lang-name") langName: String, dataType: DataType, value: ESExpr)
}

