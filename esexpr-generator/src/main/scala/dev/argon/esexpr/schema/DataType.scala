package dev.argon.esexpr.schema

import dev.argon.esexpr.*

enum DataType derives ESExprCodec, CanEqual {
  @inlineValue
  case UserDefined(name: String)

  case Apply(name: String, args: DataType*)

  case TypeEnumMatch(typeEnum: String, typeEnumValue: DataType, @dict mappings: Map[String, DataType])
  case TypeStructMember(typeStruct: String, typeStructValue: DataType, member: String)

  case List(elementType: DataType)
  case Set(elementType: DataType)
  case Dict(elementType: DataType)

  case Nullable(t: DataType)

  case Bool

  @constructor("int")
  case IntType

  @constructor("uint32")
  case UInt32

  case Int32

  @constructor("uint64")
  case UInt64

  case Int64

  @constructor("string")
  case Str

  case Binary, Float32, Float64

  @constructor("esexpr")
  case ESExpr

  @constructor("esexpr-tag")
  case ESExprTag
}
