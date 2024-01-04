package dev.argon.esexpr.schema

import dev.argon.esexpr.*

enum MethodParameter derives ESExprCodec, CanEqual {
  @constructor("arg")
  case PositionalParam(name: String, paramType: DataType)
}
