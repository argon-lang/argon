package dev.argon.esexpr.generator

import dev.argon.esexpr.ESExpr
import dev.argon.esexpr.schema.*
import org.apache.commons.text.StringEscapeUtils

trait JavaGeneratorUtil extends GeneratorBase {

  def writePackageQualifier(): Unit

  def writeInterface(name: String, `sealed`: Boolean = false, nonSealed: Boolean = false, `extends`: Seq[DataType | String] = Seq(), typeParameters: Seq[TypeParameter] = Seq())(body: => Unit): Unit =
    writeClassLike("interface", name, `sealed` = `sealed`, nonSealed = nonSealed, `extends` = `extends`, typeParameters = typeParameters)(body)

  def writeClass(name: String, `sealed`: Boolean = false, `final`: Boolean = false, `extends`: Seq[DataType | String] = Seq(), implements: Seq[DataType | String] = Seq())(body: => Unit): Unit =
    writeClassLike("class", name, `sealed` = `sealed`, `final` = `final`, `extends` = `extends`, implements = implements)(body)

  def writeRecord(name: String, implements: Seq[DataType | String] = Seq())(parameters: Parameter*)(body: => Unit): Unit =
    writeClassLike("record", name, implements = implements, primaryConstructorParameters = parameters)(body)

  def writeClassLike(
    classType: String,
    name: String,
    `sealed`: Boolean = false,
    nonSealed: Boolean = false,
    `final`: Boolean = false,
    `extends`: Seq[DataType | String] = Seq(),
    implements: Seq[DataType | String] = Seq(),
    primaryConstructorParameters: Seq[Parameter] = Seq(),
    typeParameters: Seq[TypeParameter] = Seq()
  )(body: => Unit): Unit =
    w("public ")
    if `sealed` then
      w("sealed ")
    else if nonSealed then
      w("non-sealed ")
    else if `final` then
      w("final ")
    w(classType)
    w(" ")
    writeTypeName(name)
    if typeParameters.nonEmpty then
      w("<")
      writeCommaListSingleLine(writeTypeParameter)(typeParameters)
      w(">")
    end if
    if classType == "record" then
      wl("(")
      indent {
        writeCommaListMultiline(writeParameter)(primaryConstructorParameters)
      }
      w(")")
    end if

    if `extends`.nonEmpty then
      w(" extends ")
      writeCommaListSingleLine(writeBaseTypeOrString)(`extends`)
    end if

    if implements.nonEmpty then
      w(" implements ")
      writeCommaListSingleLine(writeBaseTypeOrString)(implements)
    end if

    wl(" {")
    indent(body)
    wl("}")
  end writeClassLike


  def writeTypeParameter(p: TypeParameter): Unit =
    writeTypeName(p.name)

    p.tuple match {
      case Some(tupleName) =>
        w(" extends ")
        writePackageQualifier()
        writeTypeName(tupleName)
        w("<")
        writeTypeName(p.name)
        w(">")

      case None =>
        p.`enum` match {
          case Some(t) =>
            w(" extends ")
            writePackageQualifier()
            writeTypeName(t)

          case None =>
        }
    }
  end writeTypeParameter

  def writeParameter(p: Parameter): Unit =
    p match {
      case Parameter.KeywordParam(name, paramType, optional, _) =>
        if optional then
          w("java.util.@org.jetbrains.annotations.NotNull Optional<")
          writeTypeBoxed(paramType)
          w(">")
        else
          writeType(paramType)

        w(" ")
        writeValueName(name)

      case Parameter.Dict(name, paramType) =>
        w("java.util.Map<java.lang.String, ? extends ")
        writeType(paramType)
        w("> ")
        w(name)

      case Parameter.PositionalParam(name, paramType) =>
        writeType(paramType)
        writer.print(" ")
        writeValueName(name)

      case Parameter.VarArgParam(name, paramType) =>
        writer.print("java.util.List<? extends ")
        writeType(paramType)
        writer.print("> ")
        writeValueName(name)
    }

  override def writeType(t: DataType): Unit =
    writeTypeWithNullability(t, NullabilitySpecifier.NotNull)


  override def writeBaseType(t: DataType): Unit =
    writeTypeWithNullability(t, NullabilitySpecifier.Omit)

  enum NullabilitySpecifier derives CanEqual {
    case Omit, NotNull
  }

  private def writeNullabilitySpecifier(nullability: NullabilitySpecifier): Unit =
    nullability match {
      case NullabilitySpecifier.NotNull => w(" @org.jetbrains.annotations.NotNull ")
      case _ =>
    }

  def writeTypeWithNullability(t: DataType, nullability: NullabilitySpecifier): Unit = t match {
    case DataType.UserDefined(name) if definitions.exists(GeneratorUtil.getDefinitionName(_) == name) =>
      writePackageQualifier()
      writeNullabilitySpecifier(nullability)
      writeTypeName(name)
      if requiresErrorParam(name) then
        w("<E>")

    case DataType.UserDefined(name) =>
      writeNullabilitySpecifier(nullability)
      writeTypeName(name)
      if requiresErrorParam(name) then
        w("<E>")

    case DataType.Apply(name, args*) =>
      writePackageQualifier()
      writeNullabilitySpecifier(nullability)
      writeTypeName(name)
      w("<")
      if requiresErrorParam(name) then
        w("E, ")
      writeCommaListSingleLine(writeTypeBoxed)(args)
      w(">")

    case DataType.TypeStructMember(typeStruct, typeStructValue, member) =>
      writeTypeWithNullability(DataType.UserDefined(typeStruct), NullabilitySpecifier.Omit)
      w(".")
      writeNullabilitySpecifier(nullability)
      writeTypeName(member)
      w("<")
      writeTypeBoxed(typeStructValue)
      w(">")

    case DataType.List(elementType) =>
      w("java.util.")
      writeNullabilitySpecifier(nullability)
      w("List<? extends ")
      writeTypeBoxed(elementType)
      w(">")

    case DataType.Set(elementType) =>
      w("java.util.")
      writeNullabilitySpecifier(nullability)
      w("Set<? extends ")
      writeTypeBoxed(elementType)
      w(">")

    case DataType.Dict(elementType) =>
      w("java.util.")
      writeNullabilitySpecifier(nullability)
      w("Map<java.lang.String, ? extends ")
      writeTypeBoxed(elementType)
      w(">")

    case DataType.Nullable(t) =>
      w("java.util.")
      writeNullabilitySpecifier(nullability)
      w("Optional<")
      writeTypeBoxed(t)
      w(">")


    case DataType.Bool => w("boolean")
    case DataType.IntType =>
      w("java.math.")
      writeNullabilitySpecifier(nullability)
      w("BigInteger")
    case DataType.UInt32 | DataType.Int32 => w("int")
    case DataType.UInt64 | DataType.Int64 => w("long")
    case DataType.Str =>
      w("java.lang.")
      writeNullabilitySpecifier(nullability)
      w("String")
    case DataType.Binary =>
      w("byte")
      writeNullabilitySpecifier(nullability)
      w("[]")

    case DataType.Float32 => w("float")
    case DataType.Float64 => w("double")

    case DataType.ESExpr =>
      w("dev.argon.plugin.api.options.")
      writeNullabilitySpecifier(nullability)
      w("ESExpr")

    case DataType.ESExprTag =>
      w("dev.argon.plugin.api.options.")
      writeNullabilitySpecifier(nullability)
      w("ESExprTag")
  }

  def requiresErrorParam(name: String): Boolean =
    definitions.find(d => GeneratorUtil.getDefinitionName(d) == name)
      .exists(GeneratorUtil.isReferenceType)

  def writeTypeBoxed(t: DataType): Unit = t match {
    case DataType.Bool => w("java.lang.@org.jetbrains.annotations.NotNull Boolean")
    case DataType.UInt32 | DataType.Int32 => w("java.lang.@org.jetbrains.annotations.NotNull Integer")
    case DataType.UInt64 | DataType.Int64 => w("java.lang.@org.jetbrains.annotations.NotNull Long")
    case DataType.Float32 => w("java.lang.@org.jetbrains.annotations.NotNull Float")
    case DataType.Float64 => w("java.lang.@org.jetbrains.annotations.NotNull Double")
    case _ => writeTypeWithNullability(t, NullabilitySpecifier.NotNull)
  }

  def writeMethodParameter(p: MethodParameter): Unit =
    p match {
      case MethodParameter.PositionalParam(name, paramType) =>
        writeType(paramType)
        w(" ")
        writeValueName(name)
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
        w("new byte[] {")
        for i <- b.indices do
          w(b(i).toString)
          if i < b.length - 1 then w(", ")
        end for
        w("}")

      case ESExpr.Float32(f) => w(java.lang.Float.toHexString(f).nn)

      case ESExpr.Float64(d) => w(java.lang.Double.toHexString(d).nn)
      case ESExpr.Null => w("null")
    }
}
