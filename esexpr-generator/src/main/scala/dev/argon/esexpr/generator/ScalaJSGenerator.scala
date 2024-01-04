package dev.argon.esexpr.generator

import dev.argon.esexpr.schema.*
import dev.argon.esexpr.ESExprCodec
import dev.argon.util.{*, given}
import org.apache.commons.io.FilenameUtils
import org.apache.commons.lang3.StringUtils
import org.apache.commons.text.StringEscapeUtils
import zio.{Scope, Task, ZIO}

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

private[generator] final class ScalaJSGenerator
(
  val writer: PrintWriter,
  val definitions: Seq[Definition],
  packageName: String,
) extends GeneratorBase with ScalaGeneratorNaming {

  def writePackageQualifier(): Unit =
    w(packageName)
    w(".")
  end writePackageQualifier

  def writeConstructor(ctor: Constructor, baseTypeName: Option[String]): Unit = {
    w("trait ")
    writeTypeName(ctor.name)
    w(" extends ")
    baseTypeName match {
      case Some(baseTypeName) =>
        writeTypeName(baseTypeName)

      case None =>
        w("scala.scalajs.js.Object")
    }
    wl(" {")
    indent {
      if baseTypeName.nonEmpty then
        w("override val `type`: \"")
        w(StringEscapeUtils.escapeEcmaScript(ctor.name).nn)
        wl("\"")
      end if

      ctor.parameters.foreach(writeParameter)
    }
    wl("}")
  }

  def writeParameter(p: Parameter): Unit =
    p match {
      case Parameter.KeywordParam(name, paramType, optional, _) =>
        w("val ")
        writeValueName(name)
        w(": ")
        if optional then w("scala.scalajs.js.UndefOr[")
        writeType(paramType)
        if optional then w("]")
        wl("")

      case Parameter.Dict(name, paramType) =>
        w("val ")
        writeValueName(name)
        w(": scala.scalajs.js.Map[String, ")
        writeType(paramType)
        w("]")
        wl("")

      case Parameter.PositionalParam(name, paramType) =>
        w("val ")
        writeValueName(name)
        w(": ")
        writeType(paramType)
        wl("")

      case Parameter.VarArgParam(name, paramType) =>
        w("val ")
        writeValueName(name)
        w(": scala.scalajs.js.Array[? <: ")
        writeType(paramType)
        w("]")
        wl("")
    }

  def writeMethodParameter(p: MethodParameter): Unit =
    p match {
      case MethodParameter.PositionalParam(name, paramType) =>
        writeValueName(name)
        w(": ")
        writeType(paramType)
    }

  def writeDefinition(definition: Definition): Unit =
    definition match {
      case Definition.ConstructorDef(ctor) => writeConstructor(ctor, None)

      case Definition.Enum(name, cases*) =>
        w("sealed trait ")
        writeTypeName(name)
        wl(" extends scala.scalajs.js.Object {")
        indent {
          wl("val `type`: ")
          for (enumCase, i) <- cases.zipWithIndex do
            if i > 0 then w(" | ")
            w("\"")
            w(StringEscapeUtils.escapeJava(enumCase.name).nn)
            w("\"")
          end for
        }
        wl("}")

        w("object ")
        writeTypeName(name)
        wl(" {")
        indent {
          for enumCase <- cases do
            writeConstructor(enumCase, Some(name))
        }
        wl("}")

      case _: Definition.Const =>

      case Definition.SimpleEnum(name, values*) =>
        w("type ")
        writeTypeName(name)
        w(" = ")
        for (value, i) <- values.zipWithIndex do
          if i > 0 then w(" | ")
          w("\"")
          w(StringEscapeUtils.escapeJava(value).nn)
          w("\"")
        end for
        wl("")

      case Definition.EnumClass(name, typeParameters, cases*) =>
        withTypeParameters(typeParameters) {
          w("sealed trait ")
          writeTypeName(name)
          if typeParameters.nonEmpty then
            w("[")
            writeCommaListSingleLine(writeTypeParameter)(typeParameters)
            wl("]")
          end if
          wl(" extends scala.scalajs.js.Object {")
          indent {
            wl("val `type`: ")
            for (enumCase, i) <- cases.zipWithIndex do
              if i > 0 then w(" | ")
              w("\"")
              w(StringEscapeUtils.escapeJava(enumCase.name).nn)
              w("\"")
            end for
          }
          wl("}")

          w("object ")
          writeTypeName(name)
          wl(" {")
          indent {
            for enumCase <- cases do
              w("trait ")
              writeTypeName(enumCase.name)
              if typeParameters.nonEmpty then
                w("[")
                writeCommaListSingleLine(writeTypeParameter)(typeParameters)
                w("]")
              end if
              w(" extends ")
              writeTypeName(name)

              if typeParameters.nonEmpty then
                w("[")
                writeCommaListSingleLine[TypeParameter](tp => writeTypeName(tp.name))(typeParameters)
                w("]")
              end if
              wl(" {")

              indent {
                wl("override val `type`: ")
                w("\"")
                w(StringEscapeUtils.escapeJava(enumCase.name).nn)
                wl("\"")

                enumCase.members.foreach(writeMember)
              }
              wl("}")
            end for
          }
          wl("}")
        }

      case Definition.Interface(name, typeParameters, extendedTypes, members*) =>
        withTypeParameters(typeParameters) {
          w("trait ")
          writeTypeName(name)
          if typeParameters.nonEmpty then
            w("[")
            writeCommaListSingleLine(writeTypeParameter)(typeParameters)
            w("]")
          end if
          w(" extends ")
          if extendedTypes.nonEmpty then
            writeCommaListSingleLine(writeType)(extendedTypes)
          else
            w("scala.scalajs.js.Object")
          end if
          wl(" {")
          indent {
            members.foreach(writeMember)
          }
          wl("}")
        }

      case Definition.TypeEnum(name, values*) =>
        w("type ")
        writeTypeName(name)
        w(" = ")
        for (s, i) <- values.zipWithIndex do
          if i > 0 then w(" | ")
          w("\"")
          w(StringEscapeUtils.escapeJava(s).nn)
          w("\"")
        end for
        wl("")

      case Definition.TypeStruct(name, values*)=>
        w("type ")
        writeTypeName(name)
        w(" = ")
        for (s, i) <- values.zipWithIndex do
          if i > 0 then w(" | ")
          w("\"")
          w(StringEscapeUtils.escapeJava(s).nn)
          w("\"")
        end for
        wl("")

      case _: Definition.Extern =>
    }

  override def writeType(t: DataType): Unit =
    t match {
      case DataType.UserDefined(name) if isTypeParameter(name) =>
        writeTypeName(name)

      case DataType.UserDefined(name) =>
        writePackageQualifier()
        writeTypeName(name)

      case DataType.Apply(name, args*) =>
        writePackageQualifier()
        writeTypeName(name)
        w("[")
        writeCommaListSingleLine(writeType)(args)
        w("]")

      case DataType.List(elementType) =>
        w("scala.scalajs.js.Array[? <:")
        writeType(elementType)
        w("]")

      case DataType.Set(elementType) =>
        w("scala.scalajs.js.Set[? <:")
        writeType(elementType)
        w("]")

      case DataType.Dict(elementType) =>
        w("scala.scalajs.js.Map[String, ? <:")
        writeType(elementType)
        w("]")

      case DataType.Nullable(t) =>
        writeType(t)
        w(" | Null")

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

  def writeMember(m: Member): Unit =
    m match {
      case Member.Field(name, fieldType) =>
        w("def ")
        writeValueName(name)
        w(": ")
        writeType(fieldType)
        wl("")

      case Member.Method(name, _, async, params, returnType) =>
        w("def ")
        writeValueName(name)
        w("(")
        writeCommaListSingleLine(writeMethodParameter)(params)
        w("): ")
        if async then w("scala.scalajs.js.Promise[")
        writeType(returnType)
        if async then w("]")
        wl("")
    }

  def writeTypeParameter(p: TypeParameter): Unit =
    writeTypeName(p.name)

    p.tuple match {
      case Some(tupleName) =>
        w("[_ <: ")
        writePackageQualifier()
        writeTypeName(tupleName)
        w("]")

      case None =>
        p.`enum` match {
          case Some(t) =>
            w(" <: ")
            writePackageQualifier()
            writeTypeName(t)

          case None =>
        }
    }
  end writeTypeParameter
  
  def generate(): Unit =
    w("package ")
    wl(packageName)
    definitions.foreach(writeDefinition)
  end generate

}

object ScalaJSGenerator {

  def generate(outDir: Path, definitions: Seq[Definition], config: GeneratorConfig.ScalaJS): Task[Unit] =
    ZIO.attempt {
      val outFile = outDir.resolve(config.outFile).nn
      Files.createDirectories(outFile.getParent.nn)


      Using.resource(Files.newBufferedWriter(outFile).nn) { writer =>
        Using.resource(new PrintWriter(writer)) { writer =>
          ScalaJSGenerator(writer, definitions, config.packageName).generate()
        }
      }
    }
}
