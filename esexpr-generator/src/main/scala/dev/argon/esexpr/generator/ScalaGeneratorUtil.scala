package dev.argon.esexpr.generator

import dev.argon.esexpr.ESExpr
import dev.argon.esexpr.schema.*
import org.apache.commons.text.StringEscapeUtils

trait ScalaGeneratorUtil extends GeneratorBase {

  def writePackageQualifier(): Unit

  def writeParameter(p: Parameter): Unit =
    p match {
      case Parameter.KeywordParam(name, paramType, optional, defaultValue) =>
        w("@dev.argon.esexpr.keyword(\"" + StringEscapeUtils.escapeJava(name) + "\") ")
        writeValueName(name)
        w(": ")
        if optional then
          w("Option[")
        writeType(paramType)
        if optional then
          w("]")

        defaultValue.foreach { defVal =>
          w(" = ")
          writeValue(defVal)
        }
        wl(",")

      case Parameter.Dict(name, paramType) =>
        w("@dev.argon.esexpr.dict ")
        w(name)
        w(": Map[String, ")
        writeType(paramType)
        w("]")

      case Parameter.PositionalParam(name, paramType) =>
        writeValueName(name)
        w(": ")
        writeType(paramType)
        wl(",")

      case Parameter.VarArgParam(name, paramType) =>
        writeValueName(name)
        w(": ")
        writeType(paramType)
        wl("*")
    }


  def writeMethodParameter(p: MethodParameter): Unit =
    p match {
      case MethodParameter.PositionalParam(name, paramType) =>
        writeValueName(name)
        w(": ")
        writeType(paramType)
    }

  def requiresErrorParam(name: String): Boolean =
    definitions.find(d => GeneratorUtil.getDefinitionName(d) == name)
      .exists(GeneratorUtil.isReferenceType)


  override def writeType(t: DataType): Unit = t match {
    case DataType.UserDefined(name) if definitions.exists(GeneratorUtil.getDefinitionName(_) == name) =>
      writePackageQualifier()
      writeTypeName(name)
      if requiresErrorParam(name) then
        w("[R, E]")

    case DataType.UserDefined(name) =>
      writeTypeName(name)
      if requiresErrorParam(name) then
        w("[R, E]")

    case DataType.Apply(name, args*) =>
      writePackageQualifier()
      writeTypeName(name)
      w("[")
      if requiresErrorParam(name) then
        w("R, E, ")
      writeCommaListSingleLine(writeType)(args)
      w("]")

    case DataType.TypeEnumMatch(enumType, enumTypeCase, mappings) =>
      val enumTypeInfo = definitions.collectFirst {
        case enumTypeInfo@Definition.TypeEnum(name, _*) if enumType == name =>
          enumTypeInfo
      }.getOrElse(throw new Exception(s"Unknown enum type: $enumType"))

      if enumTypeInfo.values.size != mappings.size then
        throw new Exception(s"Invalid mappings for enum ${enumTypeInfo.name}")


      writeType(enumTypeCase)
      w(" match { ")
      for value <- enumTypeInfo.values do
        w("case \"")
        w(StringEscapeUtils.escapeJava(value).nn)
        w("\" => ")
        writeType(mappings(value))
        w(" ")
      end for
      w("}")

    case DataType.TypeStructMember(_, typeStructValue, member) =>
      writeType(typeStructValue)
      w("[\"")
      w(StringEscapeUtils.escapeJava(member).nn)
      w("\"]")

    case DataType.List(elementType) =>
      w("Seq[")
      writeType(elementType)
      w("]")

    case DataType.Set(elementType) =>
      w("Set[")
      writeType(elementType)
      w("]")

    case DataType.Dict(elementType) =>
      w("dev.argon.esexpr.Dictionary[")
      writeType(elementType)
      w("]")

    case DataType.Nullable(t) =>
      w("Option[")
      writeType(t)
      w("]")

    case DataType.Bool => w("Boolean")
    case DataType.IntType => w("BigInt")
    case DataType.UInt32 | DataType.Int32 => w("Int")
    case DataType.UInt64 | DataType.Int64 => w("Long")
    case DataType.Str => w("String")
    case DataType.Binary => w("Array[Byte]")
    case DataType.Float32 => w("Float")
    case DataType.Float64 => w("Double")
    case DataType.ESExpr => w("dev.argon.esexpr.ESExpr")
    case DataType.ESExprTag => w("dev.argon.esexpr.ESExprTag")
  }


  def writeValue(value: ESExpr): Unit =
    value match {
      case ESExpr.Constructed(constructor, kwargs, args) => ???
      case ESExpr.Bool(b) => w(b.toString)
      case ESExpr.Int(n) => w(n.toString)
      case ESExpr.Str(s) =>
        w("\"")
        w(StringEscapeUtils.escapeJava(s).nn)
        w("\"")

      case ESExpr.Binary(b) =>
        w("Array[Byte](")
        for i <- b.indices do
          w(b(i).toString)
          if i < b.length - 1 then w(", ")
        end for
        w(")")

      case ESExpr.Float32(f) =>
        w("java.lang.Float.valueOf(\"")
        w(java.lang.Float.toHexString(f).nn)
        w("\")")

      case ESExpr.Float64(d) =>
        w("java.lang.Double.valueOf(\"")
        w(java.lang.Double.toHexString(d).nn)
        w("\")")
        
      case ESExpr.Null => w("null")
    }
}
