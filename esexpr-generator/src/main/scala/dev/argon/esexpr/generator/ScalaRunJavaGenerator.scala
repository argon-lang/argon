package dev.argon.esexpr.generator

import dev.argon.esexpr.schema.*
import dev.argon.esexpr.ESExprCodec
import dev.argon.util.{*, given}
import org.apache.commons.io.FilenameUtils
import org.apache.commons.text.StringEscapeUtils
import zio.{Scope, Task, ZIO}

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

private[generator] final class ScalaRunJavaGenerator(
  val writer: PrintWriter,
  val definitions: Seq[Definition],
  packageName: String,
  scalaApiPackageName: String,
  javaApiPackageName: String,
  utilPackageName: String,
) extends GeneratorBase with ScalaGeneratorNaming with ScalaGeneratorUtil {

  private val jsValueName = "org.graalvm.polyglot.Value"

  private var nextValueNum: Int = 0

  private def getNextValueName(): String =
    val name = s"value$nextValueNum"
    nextValueNum += 1
    name
  end getNextValueName


  override def writePackageQualifier(): Unit =
    w(scalaApiPackageName)
    w(".")
  end writePackageQualifier

  def generateWrapCodec(definition: Definition): Unit =
    val definitionName = GeneratorUtil.getDefinitionName(definition)
    val definitionTypeParams = GeneratorUtil.getDefinitionTypeParameters(definition)
    val isRef = GeneratorUtil.isReferenceType(definition)


    definition match {
      case definition: Definition.TypeStruct =>
        w("final case class ")
        writeTypeName(definitionName)
        w("WrapCodec[TScala[_ <: ")
        w(scalaApiPackageName)
        w(".")
        writeTypeName(definitionName)
        w("], TJava <: ")
        w(javaApiPackageName)
        w(".")
        writeTypeName(definitionName)
        wl("[TJava]](")
        indent {
          for member <- definition.members do
            writeValueName(member)
            w("Codec: ")
            w(utilPackageName)
            w(".WrapCodec[TScala[\"")
            w(StringEscapeUtils.escapeJava(member).nn)
            w("\"], ")
            w(javaApiPackageName)
            w(".")
            writeTypeName(definitionName)
            w(".")
            writeTypeName(member)
            wl("[TJava]],")
          end for
        }
        wl(")")

      case _ =>
        def writeScalaType(): Unit =
          w(scalaApiPackageName)
          w(".")
          writeTypeName(definitionName)
          if isRef then writeTypeArgs(definitionTypeParams)
        end writeScalaType

        def writeJavaType(): Unit =
          w(javaApiPackageName)
          w(".")
          writeTypeName(definitionName)
          if isRef then writeJTypeArgs(definitionTypeParams)
        end writeJavaType

        def writeCodecBody(): Unit =
          w(s"override def wrap($reservedValueName: ")
          writeScalaType()
          w("): ")
          writeJavaType()
          wl(" =")
          indent {
            generateWrapImpl(definition)
          }

          w(s"override def unwrap($reservedValueName: ")
          writeJavaType()
          w("): ")
          writeScalaType()
          wl(" =")
          indent {
            generateUnwrapImpl(definition)
          }
        end writeCodecBody


        w("final class ")
        writeTypeName(definitionName)
        w("WrapCodec")
        if isRef then
          w("[R, E, EX <: java.lang.Throwable")
          for param <- definitionTypeParams do
            w(", ")
            writeTypeParameter(param)
          end for
          w("]")
        end if
        w("(")
        writeCommaListSingleLine[TypeParameter](param => {
          writeValueName(param.name + "-wrap-codec")
          w(": ")
          param.`enum`.orElse(param.tuple) match {
            case Some(enumName) =>
              w(packageName)
              w(".")
              writeTypeName(enumName)
              w("WrapCodec[S")
              writeTypeName(param.name)
              w(", J")
              writeTypeName(param.name)
              w("]")

            case None =>
              w(utilPackageName)
              w(".WrapCodec[S")
              writeTypeName(param.name)
              w(", J")
              writeTypeName(param.name)
              w("]")
          }
        })(definitionTypeParams)
        w(")")
        if isRef then
          w("(using effectWrapper: ")
          w(utilPackageName)
          w(".EffectWrapper[R, E, EX])")
        end if
        w(" extends ")
        w(utilPackageName)
        w(".WrapCodec[")
        writeScalaType()
        w(", ")
        writeJavaType()
        w("]")
        wl(" {")
        indent {
          writeCodecBody()
        }
        wl("}")
    }
  end generateWrapCodec

  def writeTypeParameter(p: TypeParameter): Unit =
    p.tuple match {
      case Some(tupleName) =>
        w("S")
        writeTypeName(p.name)
        w("[_ <: ")
        w(scalaApiPackageName)
        w(".")
        writeTypeName(tupleName)
        w("], J")
        writeTypeName(p.name)
        w(" <: ")
        w(javaApiPackageName)
        w(".")
        writeTypeName(tupleName)
        w("[J")
        writeTypeName(p.name)
        w("]")



      case None =>
        p.`enum` match {
          case Some(t) =>
            w("S")
            writeTypeName(p.name)
            w(" <: ")
            w(scalaApiPackageName)
            w(".")
            writeTypeName(t)
            w(", J")
            writeTypeName(p.name)
            w(" <: ")
            w(javaApiPackageName)
            w(".")
            writeTypeName(t)

          case None =>
            w("S")
            writeTypeName(p.name)
            w(", J")
            writeTypeName(p.name)
        }
    }
  end writeTypeParameter

  def generateWrapImpl(definition: Definition): Unit =
    definition match {
      case Definition.ConstructorDef(ctor) =>
        w(javaApiPackageName)
        w(".")
        writeTypeName(ctor.name)
        w("(")
        writeCommaListSingleLine(generateWrapImplConstructorParameter)(ctor.parameters)
        wl(")")

      case Definition.Enum(name, constructors*) =>
        wl(s"$reservedValueName match {")
        indent {
          for ctor <- constructors do
            w("case ")
            if ctor.parameters.nonEmpty then
              w(s"$reservedValueName: ")
            w(scalaApiPackageName)
            w(".")
            writeTypeName(name)
            w(".")
            writeTypeName(ctor.name)
            w(" => ")

            w(javaApiPackageName)
            w(".")
            writeTypeName(name)
            w(".")
            writeTypeName(ctor.name)
            w("(")
            writeCommaListSingleLine(generateWrapImplConstructorParameter)(ctor.parameters)
            wl(")")
          end for
        }
        wl("}")

      case Definition.Const(_, _, _) => throw new UnsupportedOperationException()

      case Definition.SimpleEnum(name, values*) =>
        wl(s"$reservedValueName match {")
        indent {
          for value <- values do
            w("case ")
            w(scalaApiPackageName)
            w(".")
            writeTypeName(name)
            w(".")
            writeTypeName(value)
            w(" => ")
            w(javaApiPackageName)
            w(".")
            writeTypeName(name)
            w(".")
            wl(JavaGeneratorNaming.getEnumCaseName(value))
          end for
        }
        wl("}")

      case Definition.EnumClass(name, typeParameters, cases*) =>
        withTypeParameters(typeParameters) {
          wl(s"$reservedValueName match {")
          indent {
            for enumCase <- cases do
              w(s"case $reservedValueName: ")
              w(scalaApiPackageName)
              w(".")
              writeTypeName(name)
              w(".")
              writeTypeName(enumCase.name)
              writeTypeArgs(typeParameters)
              wl(" =>")
              indent {
                w("new ")
                w(javaApiPackageName)
                w(".")
                writeTypeName(name)
                w(".")
                writeTypeName(enumCase.name)
                writeJTypeArgs(typeParameters)
                wl(" {")
                indent {
                  enumCase.members.foreach(generateWrapImplInterfaceMember)
                }
                wl("}")
              }
            end for
          }
          wl("}")
        }

      case interfaceType: Definition.Interface =>
        withTypeParameters(interfaceType.typeParameters) {
          w("new ")
          w(javaApiPackageName)
          w(".")
          writeTypeName(interfaceType.name)
          writeJTypeArgs(interfaceType.typeParameters)
          wl(" {")
          indent {
            getAllMembers(interfaceType).values.foreach(generateWrapImplInterfaceMember)
          }
          wl("}")
        }

      case Definition.TypeStruct(name, values*) => throw new UnsupportedOperationException()
      case _: Definition.Extern => throw new UnsupportedOperationException()
    }

  def generateWrapImplConstructorParameter(param: Parameter): Unit =
    writeParameterCodec(param)
    w(s".wrap($reservedValueName.")
    writeValueName(param.name)
    w(")")
  end generateWrapImplConstructorParameter

  def generateWrapImplInterfaceMember(member: Member): Unit =
    member match {
      case Member.Field(name, fieldType) =>
        w("override def ")
        w(determineJavaValueName(name))
        w("(): ")
        writeJType(fieldType)
        wl(" = ")
        indent {
          writeWrapCodecOfType(fieldType)
          w(s".wrap($reservedValueName.")
          writeValueName(name)
          wl(")")
        }

      case Member.Method(name, _, async, params, returnType) =>
        w("override def ")
        w(determineJavaValueName(name))
        w("(")
        writeCommaListSingleLine[MethodParameter] {
          case MethodParameter.PositionalParam(name, paramType) =>
            w(determineJavaValueName(name))
            w(": ")
            writeJType(paramType)
        }(params)
        w("): ")
        writeJType(returnType)
        wl(" =")
        indent {
          if async then
            w("effectWrapper.wrap(")
          else
            writeWrapCodecOfType(returnType)
            w(".wrap(")
          end if
          w(s"$reservedValueName.")
          writeValueName(name)
          w("(")
          writeCommaListSingleLine[MethodParameter] {
            case MethodParameter.PositionalParam(name, paramType) =>
              writeWrapCodecOfType(paramType)
              w(".unwrap(")
              writeValueName(name)
              w(")")
          }(params)
          w(").nn")
          if async then
            w(".map(")
            writeWrapCodecOfType(returnType)
            w(".wrap))")
          else
            w(")")
          end if
          wl("")
        }
    }


  def generateUnwrapImpl(definition: Definition): Unit =
    definition match {
      case Definition.ConstructorDef(ctor) =>
        w(scalaApiPackageName)
        w(".")
        writeTypeName(ctor.name)
        w("(")
        writeCommaListSingleLine(generateUnwrapImplConstructorParameter)(ctor.parameters)
        wl(")")

      case Definition.Enum(name, cases*) =>
        wl(s"$reservedValueName match {")
        indent {
          for ctor <- cases do
            w(s"case $reservedValueName: ")
            w(javaApiPackageName)
            w(".")
            writeTypeName(name)
            w(".")
            writeTypeName(ctor.name)
            w(" => ")

            w(scalaApiPackageName)
            w(".")
            writeTypeName(name)
            w(".")
            writeTypeName(ctor.name)

            if ctor.parameters.nonEmpty then
              w("(")
              writeCommaListSingleLine(generateUnwrapImplConstructorParameter)(ctor.parameters)
              w(")")
            end if
            wl("")
          end for
          wl(s"case _ => throw scala.MatchError($reservedValueName)")
        }
        wl("}")

      case Definition.Const(name, dataType, value) => throw new UnsupportedOperationException()
      case Definition.SimpleEnum(name, values*) =>
        wl(s"$reservedValueName match {")
        indent {
          for value <- values do
            w("case ")
            w(javaApiPackageName)
            w(".")
            writeTypeName(name)
            w(".")
            w(JavaGeneratorNaming.getEnumCaseName(value))
            w(" => ")
            w(scalaApiPackageName)
            w(".")
            writeTypeName(name)
            w(".")
            writeTypeName(value)
            wl("")
          end for
        }
        wl("}")

      case Definition.EnumClass(name, typeParameters, cases*) =>
        withTypeParameters(typeParameters) {
          wl(s"$reservedValueName match {")
          indent {
            for enumCase <- cases do
              w(s"case $reservedValueName: ")
              w(javaApiPackageName)
              w(".")
              writeTypeName(name)
              w(".")
              writeTypeName(enumCase.name)
              writeJTypeArgs(typeParameters)
              wl(" =>")
              indent {
                w("new ")
                w(scalaApiPackageName)
                w(".")
                writeTypeName(name)
                w(".")
                writeTypeName(enumCase.name)
                writeTypeArgs(typeParameters)
                wl(" {")
                indent {
                  enumCase.members.foreach(generateUnwrapImplInterfaceMember)
                }
                wl("}")
              }
            end for

            wl(s"case _ => throw new scala.MatchError($reservedValueName)")
          }
          wl("}")
        }


      case interfaceType: Definition.Interface =>
        withTypeParameters(interfaceType.typeParameters) {
          w("new ")
          w(scalaApiPackageName)
          w(".")
          writeTypeName(interfaceType.name)
          writeTypeArgs(interfaceType.typeParameters)
          wl(" {")
          indent {
            getAllMembers(interfaceType).values.foreach(generateUnwrapImplInterfaceMember)
          }
          wl("}")
        }


      case Definition.TypeStruct(name, members*) => throw new UnsupportedOperationException()
      case _: Definition.Extern => throw new UnsupportedOperationException()
    }

  def generateUnwrapImplConstructorParameter(param: Parameter): Unit =
    writeParameterCodec(param)
    w(s".unwrap($reservedValueName.")
    w(determineJavaValueName(param.name))
    w("().nn)")
    param match {
      case _: Parameter.VarArgParam =>
        w("*")

      case _ =>
    }
  end generateUnwrapImplConstructorParameter


  def writeTypeArgs(typeParameters: Seq[TypeParameter]): Unit =
    w("[R, E")
    for param <- typeParameters do
      w(", S")
      writeTypeName(param.name)
    end for
    w("]")
  end writeTypeArgs

  def writeJTypeArgs(typeParameters: Seq[TypeParameter]): Unit =
    w("[EX")
    for param <- typeParameters do
      w(", J")
      writeTypeName(param.name)
    end for
    w("]")
  end writeJTypeArgs

  def generateUnwrapImplInterfaceMember(member: Member): Unit =
    member match {
      case Member.Field(name, fieldType) =>
        w("override def ")
        writeValueName(name)
        w(": ")
        writeType(fieldType)
        wl(" = ")
        indent {
          writeWrapCodecOfType(fieldType)
          w(s".unwrap($reservedValueName.")
          w(determineJavaValueName(name))
          wl("().nn)")
        }

      case Member.Method(name, _, async, params, returnType) =>
        w("override def ")
        writeValueName(name)
        w("(")
        writeCommaListSingleLine[MethodParameter] {
          case MethodParameter.PositionalParam(name, paramType) =>
            writeValueName(name)
            w(": ")
            writeType(paramType)
        }(params)
        w("): ")
        if async then w("zio.ZIO[R, E, ")
        writeType(returnType)
        if async then w("]")
        wl(" =")
        indent {
          if async then
            w("effectWrapper.unwrap(")
          else
            writeWrapCodecOfType(returnType)
            w(".unwrap(")
          end if
          w(s"$reservedValueName.")
          w(determineJavaValueName(name))
          w("(")
          writeCommaListSingleLine[MethodParameter] {
            case MethodParameter.PositionalParam(name, paramType) =>
              writeWrapCodecOfType(paramType)
              w(".wrap(")
              writeValueName(name)
              w(")")
          }(params)
          w(").nn")
          if async then
            w(").map(")
            writeWrapCodecOfType(returnType)
            w(".unwrap)")
          else
            w(")")
          end if
          wl("")
        }
    }

  def writeParameterCodec(param: Parameter): Unit =
    param match {
      case Parameter.KeywordParam(_, paramType, true, _) =>
        writeOptionCodec(paramType)

      case Parameter.KeywordParam(_, paramType, false, _) =>
        writeWrapCodecOfType(paramType)

      case Parameter.Dict(_, paramType) =>
        writeDictCodec(paramType)

      case Parameter.PositionalParam(_, paramType) =>
        writeWrapCodecOfType(paramType)

      case Parameter.VarArgParam(_, paramType) =>
        writeSeqCodec(paramType)
    }

  def writeOptionCodec(t: DataType): Unit =
    w(utilPackageName)
    w(".WrapHelper.optionCodec(")
    writeWrapCodecOfType(t)
    w(")")
  end writeOptionCodec

  def writeDictCodec(t: DataType): Unit =
    w(utilPackageName)
    w(".WrapHelper.dictCodec(")
    writeWrapCodecOfType(t)
    w(")")
  end writeDictCodec

  def writeSeqCodec(t: DataType): Unit =
    w(utilPackageName)
    w(".WrapHelper.seqCodec(")
    writeWrapCodecOfType(t)
    w(")")
  end writeSeqCodec

  def writeWrapCodecOfType(t: DataType): Unit =
    t match {
      case DataType.UserDefined(name) if isTypeParameter(name) =>
        writeValueName(name + "-wrap-codec")

      case DataType.UserDefined(name) =>
        w(packageName)
        w(".")
        writeTypeName(name + "-wrap-codec")
        w("()")

      case DataType.Apply(name, args*) =>
        w(packageName)
        w(".")
        writeTypeName(name + "-wrap-codec")
        w("(")
        writeCommaListSingleLine(writeWrapCodecOfType)(args)
        w(")")


      case DataType.TypeStructMember(_, typeStructValue, member) =>
        writeWrapCodecOfType(typeStructValue)
        w(".")
        writeValueName(member)
        w("Codec")

      case DataType.List(elementType) =>
        writeSeqCodec(elementType)

      case DataType.Set(elementType) =>
        w(utilPackageName)
        w(".WrapHelper.setCodec(")
        writeWrapCodecOfType(elementType)
        w(")")

      case DataType.Dict(elementType) =>
        w(utilPackageName)
        w(".WrapHelper.dictCodec(")
        writeWrapCodecOfType(elementType)
        w(")")

      case DataType.Nullable(t) =>
        writeOptionCodec(t)

      case DataType.Bool =>
        w(utilPackageName)
        w(".WrapHelper.boolCodec")

      case DataType.IntType =>
        w(utilPackageName)
        w(".WrapHelper.intCodec")

      case DataType.UInt32 => ???
      case DataType.Int32 => ???
      case DataType.UInt64 => ???
      case DataType.Int64 => ???
      case DataType.Str =>
        w(utilPackageName)
        w(".WrapHelper.identityCodec[String]")

      case DataType.Binary => ???
      case DataType.Float32 => ???
      case DataType.Float64 => ???
      case DataType.ESExpr =>
        w(utilPackageName)
        w(".WrapHelper.exprCodec")

      case DataType.ESExprTag =>
        w(utilPackageName)
        w(".WrapHelper.exprTagCodec")
    }


  override def writeType(t: DataType): Unit =
    t match {
      case DataType.UserDefined(name) if isTypeParameter(name) =>
        w("S")
        writeTypeName(name)

      case _ => super.writeType(t)
    }

  def writeJType(t: DataType): Unit = t match {
    case DataType.UserDefined(name) if isTypeParameter(name) =>
      w("J")
      writeTypeName(name)

    case DataType.UserDefined(name) =>
      w(javaApiPackageName)
      w(".")
      writeTypeName(name)
      if requiresErrorParam(name) then
        w("[EX]")

    case DataType.Apply(name, args*) =>
      w(javaApiPackageName)
      w(".")
      writeTypeName(name)
      w("[")
      if requiresErrorParam(name) then
        w("EX, ")
      writeCommaListSingleLine(writeJType)(args)
      w("]")

    case DataType.TypeStructMember(typeStruct, typeStructValue, member) =>
      writeJType(DataType.UserDefined(typeStruct))
      w(".")
      writeTypeName(member)
      w("[")
      writeJType(typeStructValue)
      w("]")

    case DataType.List(elementType) =>
      w("java.util.List[? <: ")
      writeJType(elementType)
      w("]")

    case DataType.Set(elementType) =>
      w("java.util.Set[? <: ")
      writeJType(elementType)
      w("]")

    case DataType.Dict(elementType) =>
      w("java.util.Map[String, ? <: ")
      writeJType(elementType)
      w("]")

    case DataType.Nullable(t) =>
      w("java.util.Optional[")
      writeJType(t)
      w("]")


    case DataType.Bool => w("Boolean")
    case DataType.IntType => w("java.math.BigInteger")
    case DataType.UInt32 | DataType.Int32 => w("Int")
    case DataType.UInt64 | DataType.Int64 => w("Long")
    case DataType.Str => w("java.lang.String")
    case DataType.Binary => w("Array[Byte]")
    case DataType.Float32 => w("Float")
    case DataType.Float64 => w("Double")
    case DataType.ESExpr => w("dev.argon.plugin.api.options.ESExpr")
    case DataType.ESExprTag => w("dev.argon.plugin.api.options.ESExprTag")
  }


  def generate(): Unit =
    w("package ")
    wl(packageName)
    wl("import dev.argon.util.{*, given}")
    definitions.filter {
      case _: Definition.Const => false
      case _: Definition.Extern => false
      case _ => true
    }.foreach(generateWrapCodec)
  end generate

}

object ScalaRunJavaGenerator {
  def generate(outDir: Path, definitions: Seq[Definition], config: GeneratorConfig.ScalaRunJava): Task[Unit] =
    ZIO.attempt {
      val outFile = outDir.resolve(config.outFile).nn
      Files.createDirectories(outFile.getParent.nn)
      Using.resource(Files.newBufferedWriter(outFile).nn) { writer =>
        Using.resource(new PrintWriter(writer)) { writer =>
          ScalaRunJavaGenerator(writer, definitions, config.packageName, config.scalaApiPackageName, config.javaApiPackageName, config.utilPackageName).generate()
        }
      }
    }
}
