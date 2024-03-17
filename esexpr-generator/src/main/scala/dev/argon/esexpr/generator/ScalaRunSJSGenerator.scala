package dev.argon.esexpr.generator

import dev.argon.esexpr.schema.*
import dev.argon.esexpr.ESExprCodec
import dev.argon.util.{*, given}
import org.apache.commons.io.FilenameUtils
import org.apache.commons.text.StringEscapeUtils
import zio.{Scope, ZIO}

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

private[generator] final class ScalaRunSJSGenerator(
  val writer: PrintWriter,
  val definitions: Seq[Definition],
  packageName: String,
  scalaApiPackageName: String,
  jsApiPackageName: String,
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
        w("], TJS[_ <: ")
        w(jsApiPackageName)
        w(".")
        writeTypeName(definitionName)
        wl("]](")
        indent {
          for member <- definition.members do
            writeValueName(member)
            w("Codec: ")
            w(utilPackageName)
            w(".WrapCodec[TScala[\"")
            w(StringEscapeUtils.escapeJava(member).nn)
            w("\"], ")
            w("TJS[\"")
            w(StringEscapeUtils.escapeJava(member).nn)
            wl("\"]],")
          end for
        }
        wl(")")

      case _: Definition.Extern =>

      case _ =>
        def writeScalaType(): Unit =
          w(scalaApiPackageName)
          w(".")
          writeTypeName(definitionName)
          if isRef then writeTypeArgs(definitionTypeParams)
        end writeScalaType

        def writeJavaType(): Unit =
          w(jsApiPackageName)
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
          w("[R, E")
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
          w(".EffectWrapper[R, E])")
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
        w("[_ <: ")
        w(jsApiPackageName)
        w(".")
        writeTypeName(tupleName)
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
            w(jsApiPackageName)
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
        w("new ")
        w(jsApiPackageName)
        w(".")
        writeTypeName(ctor.name)
        wl(" {")
        indent {
          ctor.parameters.foreach(generateWrapImplConstructorParameter)
        }
        wl("}")

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
            wl(" => ")

            indent {
              w("new ")
              w(jsApiPackageName)
              w(".")
              writeTypeName(name)
              w(".")
              writeTypeName(ctor.name)
              wl(" {")
              indent {
                w("override val `type`: \"")
                w(StringEscapeUtils.escapeJava(ctor.name).nn)
                w("\" = \"")
                w(StringEscapeUtils.escapeJava(ctor.name).nn)
                wl("\"")
                ctor.parameters.foreach(generateWrapImplConstructorParameter)
              }
              wl("}")
            }
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
            w(" => \"")
            w(StringEscapeUtils.escapeJava(value).nn)
            wl("\"")
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
                w(jsApiPackageName)
                w(".")
                writeTypeName(name)
                w(".")
                writeTypeName(enumCase.name)
                writeJTypeArgs(typeParameters)
                wl(" {")
                indent {
                  w("override val `type`: \"")
                  w(StringEscapeUtils.escapeJava(enumCase.name).nn)
                  w("\" = \"")
                  w(StringEscapeUtils.escapeJava(enumCase.name).nn)
                  wl("\"")
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
          w(jsApiPackageName)
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
    w("override val ")
    writeValueName(param.name)
    w(": ")
    param match {
      case Parameter.KeywordParam(_, paramType, true, _) =>
        w("scala.scalajs.js.UndefOr[")
        writeJSType(paramType)
        w("]")

      case Parameter.KeywordParam(_, paramType, false, _) =>
        writeJSType(paramType)

      case Parameter.Dict(_, paramType) =>
        w("scala.scalajs.js.Map[String, ")
        writeJSType(paramType)
        w("]")

      case Parameter.PositionalParam(_, paramType) =>
        writeJSType(paramType)

      case Parameter.VarArgParam(_, paramType) =>
        w("scala.scalajs.js.Array[? <: ")
        writeJSType(paramType)
        w("]")
    }
    w(" = ")

    writeParameterCodec(param)
    w(s".wrap($reservedValueName.")
    writeValueName(param.name)
    w(")")
    wl("")
  end generateWrapImplConstructorParameter

  def generateWrapImplInterfaceMember(member: Member): Unit =
    member match {
      case Member.Field(name, fieldType) =>
        w("override def ")
        writeValueName(name)
        w(": ")
        writeJSType(fieldType)
        wl(" = ")
        indent {
          writeWrapCodecOfType(fieldType)
          w(s".wrap($reservedValueName.")
          writeValueName(name)
          wl(")")
        }

      case Member.Method(name, _, async, params, returnType) =>
        w("override def ")
        writeValueName(name)
        w("(")
        writeCommaListSingleLine[MethodParameter] {
          case MethodParameter.PositionalParam(name, paramType) =>
            writeValueName(name)
            w(": ")
            writeJSType(paramType)
        }(params)
        w("): ")
        if async then w("scala.scalajs.js.Promise[")
        writeJSType(returnType)
        if async then w("]")
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
          w(")")
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
        writeCommaListSingleLine(generateUnwrapImplConstructorParameter(reservedValueName))(ctor.parameters)
        wl(")")

      case Definition.Enum(name, cases*) =>
        wl(s"$reservedValueName.`type` match {")
        indent {
          for ctor <- cases do
            w("case \"")
            w(StringEscapeUtils.escapeJava(ctor.name).nn)
            wl("\" =>")
            indent {
              val valueName = getNextValueName()
              w("val ")
              w(valueName)
              w(s" = $reservedValueName.asInstanceOf[")
              w(jsApiPackageName)
              w(".")
              writeTypeName(name)
              w(".")
              writeTypeName(ctor.name)
              wl("]")


              w(scalaApiPackageName)
              w(".")
              writeTypeName(name)
              w(".")
              writeTypeName(ctor.name)

              if ctor.parameters.nonEmpty then
                w("(")
                writeCommaListSingleLine(generateUnwrapImplConstructorParameter(valueName))(ctor.parameters)
                w(")")
              end if
              wl("")
            }
          end for
        }
        wl("}")

      case Definition.Const(name, dataType, value) => throw new UnsupportedOperationException()
      case Definition.SimpleEnum(name, values*) =>
        wl(s"$reservedValueName match {")
        indent {
          for value <- values do
            w("case \"")
            w(StringEscapeUtils.escapeJava(value).nn)
            w("\" => ")
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
          wl(s"$reservedValueName.`type` match {")
          indent {
            for enumCase <- cases do
              w("case \"")
              w(StringEscapeUtils.escapeJava(enumCase.name).nn)
              wl("\" =>")
              indent {
                val valueName = getNextValueName()
                w("val ")
                w(valueName)
                w(s" = $reservedValueName.asInstanceOf[")
                w(jsApiPackageName)
                w(".")
                writeTypeName(name)
                w(".")
                writeTypeName(enumCase.name)
                writeJTypeArgs(typeParameters)
                wl("]")

                w("new ")
                w(scalaApiPackageName)
                w(".")
                writeTypeName(name)
                w(".")
                writeTypeName(enumCase.name)
                writeTypeArgs(typeParameters)
                wl(" {")
                indent {
                  enumCase.members.foreach(generateUnwrapImplInterfaceMember(valueName))
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
          w(scalaApiPackageName)
          w(".")
          writeTypeName(interfaceType.name)
          writeTypeArgs(interfaceType.typeParameters)
          wl(" {")
          indent {
            getAllMembers(interfaceType).values.foreach(generateUnwrapImplInterfaceMember(reservedValueName))
          }
          wl("}")
        }


      case Definition.TypeStruct(name, members*) => throw new UnsupportedOperationException()
      case _: Definition.Extern => throw new UnsupportedOperationException()
    }

  def generateUnwrapImplConstructorParameter(valueName: String)(param: Parameter): Unit =
    writeParameterCodec(param)
    w(".unwrap(")
    w(valueName)
    w(".")
    writeValueName(param.name)
    w(")")
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
    if typeParameters.nonEmpty then
      w("[")
      writeCommaListSingleLine[TypeParameter](param => {
        w("J")
        writeTypeName(param.name)
      })(typeParameters)
      w("]")
    end if

  def generateUnwrapImplInterfaceMember(valueName: String)(member: Member): Unit =
    member match {
      case Member.Field(name, fieldType) =>
        w("override def ")
        writeValueName(name)
        w(": ")
        writeType(fieldType)
        wl(" = ")
        indent {
          writeWrapCodecOfType(fieldType)
          w(".unwrap(")
          w(valueName)
          w(".")
          writeValueName(name)
          wl(")")
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
          w(valueName)
          w(".")
          writeValueName(name)
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
        w(utilPackageName)
        w(".WrapHelper.optionUndefinedCodec(")
        writeWrapCodecOfType(paramType)
        w(")")

      case Parameter.KeywordParam(_, paramType, false, _) =>
        writeWrapCodecOfType(paramType)

      case Parameter.Dict(_, paramType) =>
        writeDictCodec(paramType)

      case Parameter.PositionalParam(_, paramType) =>
        writeWrapCodecOfType(paramType)

      case Parameter.VarArgParam(_, paramType) =>
        writeSeqCodec(paramType)
    }

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
        w(utilPackageName)
        w(".WrapHelper.optionNullCodec(")
        writeWrapCodecOfType(t)
        w(")")

      case DataType.Bool =>
        w(utilPackageName)
        w(".WrapHelper.identityCodec[Boolean]")

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

  def writeJSType(t: DataType): Unit = t match {
    case DataType.UserDefined(name) if isTypeParameter(name) =>
      w("J")
      writeTypeName(name)

    case DataType.UserDefined(name) =>
      w(jsApiPackageName)
      w(".")
      writeTypeName(name)

    case DataType.Apply(name, args*) =>
      w(jsApiPackageName)
      w(".")
      writeTypeName(name)
      w("[")
      writeCommaListSingleLine(writeJSType)(args)
      w("]")

    case DataType.TypeStructMember(_, typeStructValue, member) =>
      writeJSType(typeStructValue)
      w("[\"")
      w(StringEscapeUtils.escapeJava(member).nn)
      w("\"]")

    case DataType.List(elementType) =>
      w("scala.scalajs.js.Array[? <: ")
      writeJSType(elementType)
      w("]")

    case DataType.Set(elementType) =>
      w("scala.scalajs.js.Set[? <: ")
      writeJSType(elementType)
      w("]")

    case DataType.Dict(elementType) =>
      w("scala.scalajs.js.Map[String, ? <: ")
      writeJSType(elementType)
      w("]")

    case DataType.Nullable(t) =>
      w("(")
      writeJSType(t)
      w(") | Null")

    case DataType.Bool => w("Boolean")
    case DataType.IntType => w("scala.scalajs.js.BigInt")
    case DataType.UInt32 => w("Double")
    case DataType.Int32 => w("Int")
    case DataType.UInt64 | DataType.Int64 => w("scala.scalajs.js.BigInt")
    case DataType.Str => w("String")
    case DataType.Binary => w("scala.scalajs.js.typedarray.Uint8Array")
    case DataType.Float32 => w("Float")
    case DataType.Float64 => w("Double")
    case DataType.ESExpr => w("dev.argon.plugin.jsapi.options.ESExpr")
    case DataType.ESExprTag => w("dev.argon.plugin.jsapi.options.ESExprTag")
  }

  def generate(): Unit =
    w("package ")
    wl(packageName)
    wl("import dev.argon.util.{*, given}")
    definitions.filter {
      case _: Definition.Const => false
      case _ => true
    }.foreach(generateWrapCodec)
  end generate

}

object ScalaRunSJSGenerator {

  def generate(outDir: Path, definitions: Seq[Definition], config: GeneratorConfig.ScalaRunSJS): ZIO[Scope, Any, Unit] =
    ZIO.attempt {
      val outFile = outDir.resolve(config.outFile).nn
      Files.createDirectories(outFile.getParent.nn)
      Using.resource(Files.newBufferedWriter(outFile).nn) { writer =>
        Using.resource(new PrintWriter(writer)) { writer =>
          ScalaRunSJSGenerator(writer, definitions, config.packageName, config.scalaApiPackageName, config.jsApiPackageName, config.utilPackageName).generate()
        }
      }
    }
  end generate
}
