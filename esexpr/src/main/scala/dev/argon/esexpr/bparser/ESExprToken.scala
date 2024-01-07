package dev.argon.esexpr.bparser

import zio.Chunk

enum ESExprToken derives CanEqual {
  case ConstructorStart(nameIndex: BigInt)
  case ConstructorEnd
  case IntValue(value: BigInt)
  case StringValue(value: String)
  case StringPoolValue(index: BigInt)
  case BytesValue(value: Chunk[Byte])
  case KeywordArgument(nameIndex: BigInt)
  case Float32Value(value: Float)
  case Float64Value(value: Double)
  case BooleanValue(value: Boolean)
  case NullValue
}
