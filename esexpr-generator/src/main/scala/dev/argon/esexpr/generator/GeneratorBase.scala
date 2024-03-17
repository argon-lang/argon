package dev.argon.esexpr.generator

import dev.argon.esexpr.schema.{DataType, Definition, Member, MethodParameter, TypeParameter}
import org.apache.commons.lang3.StringUtils
import dev.argon.util.{*, given}

import java.io.PrintWriter

private[generator] abstract class GeneratorBase extends GeneratorNaming {
  
  val reservedValueName = "esexpr_reserved_value"

  val writer: PrintWriter
  val definitions: Seq[Definition]

  private var indentLevel = 0
  private var needIndent = false

  final def indent(f: => Unit): Unit =
    indentLevel += 1
    f
    indentLevel -= 1
  end indent

  final def w(s: String): Unit =
    if needIndent then
      needIndent = false
      writer.print("\t" * indentLevel)
    end if
    writer.print(s)
  end w

  def wl(s: String): Unit =
    if needIndent then
      writer.print("\t" * indentLevel)

    writer.println(s)
    needIndent = true
  end wl


  private var typeParameters: Map[String, TypeParameter] = Map.empty
  final def withTypeParameters(params: Seq[TypeParameter])(f: => Unit): Unit =
    val oldTypeParams = typeParameters
    typeParameters = typeParameters ++ params.map(p => p.name -> p)
    try f
    finally typeParameters = oldTypeParams
  end withTypeParameters

  final def isTypeParameter(param: String): Boolean =
    typeParameters.contains(param)

  final def getTypeParameter(param: String): Option[TypeParameter] =
    typeParameters.get(param)




  def writeType(t: DataType): Unit
  def writeBaseType(t: DataType): Unit = writeType(t)

  final def writeBaseTypeOrString(t: DataType | String): Unit =
    t match {
      case t: DataType => writeBaseType(t)
      case t: String => w(t)
    }

  protected def determineConstantName(name: String): String =
    name.split("-").nn
      .map(_.nn.toUpperCase.nn)
      .mkString("_")


  protected def writeCommaListMultiline[A](f: A => Unit)(s: Seq[A]): Unit =
    s.zipWithIndex.foreach { (a, i) =>
      f(a)

      if i < s.size - 1 then
        wl(",")
      else
        wl("")
    }

  protected def writeCommaListSingleLine[A](f: A => Unit)(s: Seq[A]): Unit =
    s.zipWithIndex.foreach { (a, i) =>
      f(a)

      if i < s.size - 1 then
        w(", ")
    }

  protected def writeTypeName(name: String): Unit =
    w(determineTypeName(name))

  protected def writeValueName(name: String): Unit =
    w(determineValueName(name))


  def getAllMembers(interfaceType: Definition.Interface): Map[String, Member] =
    val baseMembers = interfaceType.`extends`.flatMap(getAllMembersBase).toMap
    baseMembers ++ interfaceType.members.map(member => member.name -> member)
  end getAllMembers

  def getAllMembersBase(t: DataType): Map[String, Member] =
    val (tName, tArgs) = t match {
      case DataType.UserDefined(name) => (name, Seq())
      case DataType.Apply(name, args*) => (name, args)
      case _ => throw new Exception(s"Invalid base type: $t")
    }

    val interfaceType = definitions.find(d => GeneratorUtil.getDefinitionName(d) == tName) match {
      case Some(i: Definition.Interface) => i
      case Some(_) => throw new Exception(s"Invalid base type: $tName must be an interface")
      case None => throw new Exception(s"Invalid base type: $tName was not defined")
    }

    if tArgs.size != interfaceType.typeParameters.size then
      throw new Exception(s"Interface ${tName} used with wrong number of arguments (expected: ${interfaceType.typeParameters.size}, actual: ${tArgs.size})")

    val typeSubst = interfaceType.typeParameters.map(_.name).zip(tArgs).toMap
    getAllMembers(interfaceType).view.mapValues(substituteMemberTypes(typeSubst)).toMap
  end getAllMembersBase

  def substituteMemberTypes(typeSubst: Map[String, DataType])(member: Member): Member =
    member match {
      case Member.Field(name, fieldType) => Member.Field(name, substituteType(typeSubst)(fieldType))
      case Member.Method(name, isOverride, async, params, returnType) =>
        Member.Method(
          name,
          isOverride,
          async,
          params.map {
            case MethodParameter.PositionalParam(name, paramType) =>
              MethodParameter.PositionalParam(name, substituteType(typeSubst)(paramType))
          },
          substituteType(typeSubst)(returnType)
        )
    }

  def substituteType(typeSubst: Map[String, DataType])(t: DataType): DataType =
    t match {
      case DataType.UserDefined(name) =>
        typeSubst.get(name) match {
          case Some(t2) => t2
          case None => t
        }

      case DataType.Apply(name, args*) =>
        DataType.Apply(name, args.map(substituteType(typeSubst)) *)

      case DataType.TypeStructMember(typeStruct, typeStructValue, member) =>
        DataType.TypeStructMember(typeStruct, substituteType(typeSubst)(typeStructValue), member)

      case DataType.List(elementType) => DataType.List(substituteType(typeSubst)(elementType))
      case DataType.Set(elementType) => DataType.Set(substituteType(typeSubst)(elementType))
      case DataType.Dict(elementType) => DataType.Dict(substituteType(typeSubst)(elementType))
      case DataType.Nullable(t) => DataType.Nullable(substituteType(typeSubst)(t))
      case DataType.Bool => DataType.Bool
      case DataType.IntType => DataType.IntType
      case DataType.UInt32 => DataType.UInt32
      case DataType.Int32 => DataType.Int32
      case DataType.UInt64 => DataType.UInt64
      case DataType.Int64 => DataType.Int64
      case DataType.Str => DataType.Str
      case DataType.Binary => DataType.Binary
      case DataType.Float32 => DataType.Float32
      case DataType.Float64 => DataType.Float64
      case DataType.ESExpr => DataType.ESExpr
      case DataType.ESExprTag => DataType.ESExprTag
    }

}

object GeneratorBase {

  def determineNamePascalCase(name: String): String =
    name.split("-").nn
      .map(StringUtils.capitalize)
      .mkString
}

