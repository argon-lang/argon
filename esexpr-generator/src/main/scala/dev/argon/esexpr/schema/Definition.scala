package dev.argon.esexpr.schema

import dev.argon.esexpr.*

enum Definition derives ESExprCodec {
  @inlineValue
  case ConstructorDef(constructor: Constructor)
  case Enum(name: String, cases: Constructor*)
  case Const(name: String, dataType: DataType, value: ESExpr)
  case SimpleEnum(name: String, values: String*)

  case EnumClass(
    name: String,

    @keyword("type-parameters")
    @defaultValue(Seq.empty)
    typeParameters: Seq[TypeParameter],

    cases: EnumCase*
  )
  
  case Interface(
    name: String,
    
    @keyword("type-parameters")
    @defaultValue(Seq.empty)
    typeParameters: Seq[TypeParameter],
    
    @keyword
    @defaultValue(Seq.empty)
    `extends`: Seq[DataType],
  
    members: Member*
  )

  case TypeStruct(name: String, members: String*)

  case Extern(
    name: String,

    @keyword("type-parameters")
    @defaultValue(Seq.empty)
    typeParameters: Seq[TypeParameter],
  )
}

