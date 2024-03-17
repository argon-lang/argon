package dev.argon.esexpr.generator

import dev.argon.esexpr.{ESExprCodec, generator}
import dev.argon.esexpr.schema.*
import dev.argon.util.{*, given}
import org.apache.commons.text.StringEscapeUtils

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.util.Using
import scala.jdk.CollectionConverters.*
import org.apache.commons.io.FilenameUtils
import org.apache.commons.lang3.StringUtils
import zio.*

private[generator] final class ScalaGenerator
(
  val writer: PrintWriter,
  val definitions: Seq[Definition],
  packageName: String,
) extends GeneratorBase with ScalaGeneratorNaming with ScalaGeneratorUtil {

  override def writePackageQualifier(): Unit =
    w(packageName)
    w(".")
  end writePackageQualifier




  def writeTrait(name: String, `sealed`: Boolean = false, `extends`: Seq[DataType] = Seq(), annotations: Seq[String] = Seq())(body: => Unit): Unit =
    writeClassLike("trait", name, `sealed` = `sealed`, `extends` = `extends`, annotations = annotations)(body)

  def writeClass(name: String, `sealed`: Boolean = false, `extends`: Seq[DataType] = Seq(), annotations: Seq[String] = Seq())(body: => Unit): Unit =
    writeClassLike("class", name, `sealed` = `sealed`, `extends` = `extends`, annotations = annotations)(body)

  def writeCaseClass(name: String, `extends`: Seq[DataType] = Seq.empty, annotations: Seq[String] = Seq())(parameters: Parameter*)(body: => Unit): Unit =
    writeClassLike("case class", name, `extends` = `extends`, primaryConstructorParameters = parameters, annotations = annotations)(body)

  def writeEnum(name: String, `extends`: Seq[DataType] = Seq.empty, annotations: Seq[String] = Seq())(body: => Unit): Unit =
    writeClassLike("enum", name, `extends` = `extends`, annotations = annotations)(body)

  def writeClassLike
  (
    classType: String,
    name: String,
    `sealed`: Boolean = false,
    `extends`: Seq[DataType | String] = Seq(),
    `derives`: Seq[DataType] = Seq(),
    primaryConstructorParameters: Seq[Parameter] = Seq.empty,
    annotations: Seq[String] = Seq(),
  )(body: => Unit): Unit =
    for ann <- annotations do
      w(ann)
      w(" ")
    end for

    if `sealed` then
      w("sealed ")
    w(classType)
    w(" ")
    writeTypeName(name)
    if primaryConstructorParameters.nonEmpty then
      wl("(")
      indent {
        writeCommaListMultiline(writeParameter)(primaryConstructorParameters)
      }
      w(")")
    end if

    if `extends`.nonEmpty then
      w("extends ")
      writeCommaListSingleLine(writeBaseTypeOrString)(`extends`)
    end if

    if `derives`.nonEmpty then
      w("derives ")
      writeCommaListSingleLine(writeBaseTypeOrString)(`derives`)
    end if

    wl(" {")
    indent(body)
    wl("}")
  end writeClassLike

  def writeConstructor(ctor: Constructor, isCase: Boolean): Unit = {
    w("@dev.argon.esexpr.constructor(\"")
    w(StringEscapeUtils.escapeJava(ctor.name).nn)
    wl("\")")

    if ctor.inlineValue.getOrElse(false) then
      wl("@dev.argon.esexpr.inlineValue")
    end if


    if isCase then
      w("case ")
    else if ctor.parameters.nonEmpty then
      w("final case class ")
    else
      w("case object ")


    writeTypeName(ctor.name)
    if ctor.parameters.nonEmpty then
      wl("(")
      ctor.parameters.foreach(writeParameter)
      w(")")
    end if

    if !isCase then
      w(" derives dev.argon.esexpr.ESExprCodec, scala.CanEqual")

    wl("")
  }

  def writeDefinition(definition: Definition): Unit =
    definition match {
      case Definition.ConstructorDef(ctor) => writeConstructor(ctor, false)

      case Definition.Enum(name, cases*) =>
        w("enum ")
        writeTypeName(name)
        wl(" derives dev.argon.esexpr.ESExprCodec, scala.CanEqual {")

        indent {
          for enumCase <- cases do
            writeConstructor(enumCase, true)
        }

        wl("}")

      case Definition.Const(name, dataType, value) =>
        w("val ")
        writeValueName(name)
        w(": ")
        writeType(dataType)
        w(" = ")
        writeValue(value)
        wl("")

      case Definition.SimpleEnum(name, values*) =>
        w("@dev.argon.esexpr.simple enum ")
        writeTypeName(name)
        wl(" derives dev.argon.esexpr.ESExprCodec, scala.CanEqual {")
        indent {
          for value <- values do
            w("@dev.argon.esexpr.caseValue(\"")
            w(StringEscapeUtils.escapeJava(value).nn)
            w("\") case ")
            writeTypeName(value)
            wl("")
          end for
        }
        wl("}")

      case Definition.EnumClass(name, typeParameters, cases*) =>
        w("sealed trait ")
        writeTypeName(name)
        w("[R, E")
        for tp <- typeParameters do
          w(", ")
          writeTypeParameter(tp)
        end for
        wl("]")

        w("object ")
        writeTypeName(name)
        wl(" {")
        indent {
          for enumCase <- cases do
            w("trait ")
            writeTypeName(enumCase.name)
            w("[R, E")
            for tp <- typeParameters do
              w(", ")
              writeTypeParameter(tp)
            end for
            w("] extends ")
            writeTypeName(name)
            w("[R, E")
            for tp <- typeParameters do
              w(", ")
              writeTypeName(tp.name)
            end for
            wl("] {")
            indent {
              enumCase.members.foreach(writeMember)
            }
            wl("}")
          end for
        }
        wl("}")

      case Definition.Interface(name, typeParameters, extendedTypes, members*) =>
        w("trait ")
        writeTypeName(name)
        w("[R, E")
        for tp <- typeParameters do
          w(", ")
          writeTypeParameter(tp)
        end for
        w("]")
        if extendedTypes.nonEmpty then
          w(" extends ")
          writeCommaListSingleLine(writeType)(extendedTypes)
        end if
        wl(" {")
        indent {
          members.foreach(writeMember)
        }
        wl("}")

      case Definition.TypeStruct(name, values*) =>
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
        if async then w("zio.ZIO[R, E, ")
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

object ScalaGenerator {
  def generate(outDir: Path, definitions: Seq[Definition], config: GeneratorConfig.Scala): Task[Unit] =
    ZIO.attempt {
      val outFile = outDir.resolve(config.outFile).nn
      Files.createDirectories(outFile.getParent.nn)

      Using.resource(Files.newBufferedWriter(outFile).nn) { writer =>
        Using.resource(new PrintWriter(writer)) { writer =>
          ScalaGenerator(writer, definitions, config.packageName).generate()
        }
      }
    }
}
