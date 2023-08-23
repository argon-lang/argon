package dev.argon.esexpr.generator

import dev.argon.esexpr.{ESExprCodec, ESExprTextReader}
import dev.argon.esexpr.schema.*
import dev.argon.util.{*, given}
import org.apache.commons.text.StringEscapeUtils

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.util.Using
import scala.jdk.CollectionConverters.*
import ScalaGeneratorJ.*
import org.apache.commons.lang3.StringUtils

object ScalaGenerator {

  def main(args: Array[String]): Unit =
    args match {
      case Array(esxFile, outFile, packageName) =>
        val exprs = Using.resource(Files.newBufferedReader(Path.of(esxFile)).nn) { reader =>
          ESExprTextReader(reader).readAll().asScala.toSeq
        }

        val definitions = exprs.traverse(summon[ESExprCodec[Definition]].decode)
          .left.map(new Exception(_))
          .toTry
          .get

        Using.resource(Files.newBufferedWriter(Path.of(outFile)).nn) { writer =>
          Using.resource(new PrintWriter(writer)) { writer =>
            generate(writer, definitions, packageName)
          }
        }

      case _ =>
        println("Invalid args")
        System.exit(1)
    }

  private def determineName(name: String): String =
    name.split("-").nn
      .map(StringUtils.capitalize)
      .mkString

  private def determineNameLower(name: String): String =
    StringUtils.uncapitalize(
      name.split("-").nn
        .map(StringUtils.capitalize)
        .mkString
    ).nn

  def generate(writer: PrintWriter, definitions: Seq[Definition], packageName: String): Unit =
    writer.print("package ")
    writer.println(packageName)

    val nameMap = definitions.flatMap(buildNameMap).toMap

    def writeType(t: DataType): Unit = t match {
      case DataType.UserDefined(name) => writer.write(nameMap(name))

      case DataType.List(elementType) =>
        writer.write("Seq[")
        writeType(elementType)
        writer.write("]")

      case DataType.Bool => writer.write("Boolean")
      case DataType.IntType => writer.write("BigInt")
      case DataType.UInt32 | DataType.Int32 => writer.write("Int")
      case DataType.UInt64 | DataType.Int64 => writer.write("Long")
      case DataType.Str => writer.write("String")
      case DataType.Binary => writer.write("Array[Byte]")
      case DataType.Float32 => writer.write("Float")
      case DataType.Float64 => writer.write("Double")
    }

    def writeConstructor(ctor: Constructor, isCase: Boolean): Unit = {
      if isCase then writer.print("  ")
      writer.print("@constructor(\"")
      writer.print(StringEscapeUtils.escapeJava(ctor.name))
      writer.println("\")")

      if isCase then
        writer.print("  case ")
      else if ctor.parameters.nonEmpty then
        writer.print("final case class ")
      else
        writer.print("case object ")


      writer.print(ctor.langName.getOrElse(determineName(ctor.name)))
      if ctor.parameters.nonEmpty then
        writer.println("(")

        ctor.parameters.foreach {
          case Parameter.KeywordParam(name, langName, paramType, optional, defaultValue) =>
            if isCase then writer.print("  ")
            writer.print("  @keyword")
            if langName.isEmpty || name != determineNameLower(name) then
              writer.print("(\"" + StringEscapeUtils.escapeJava(name) + "\")")
            writer.print(" ")
            writer.print(langName.getOrElse(determineNameLower(name)))
            writer.print(": ")
            if optional then
              writer.print("Option[")
            writeType(paramType)
            if optional then
              writer.print("]")

            defaultValue.foreach { defVal =>
              writer.print(" = ")
              writeValue(writer, defVal)
            }
            writer.println(",")

          case Parameter.PositionalParam(name, paramType) =>
            if isCase then writer.print("  ")
            writer.print("  ")
            writer.print(name)
            writer.print(": ")
            writeType(paramType)
            writer.println(",")

          case Parameter.VarArgParam(name, paramType) =>
            if isCase then writer.print("  ")
            writer.print("  ")
            writer.print(name)
            writer.print(": ")
            writeType(paramType)
            writer.println("*")
        }

        if isCase then writer.print("  ")
        writer.print(")")
      end if

      if !isCase then
        writer.print(" derives dev.argon.esexpr.ESExprCodec")

      writer.println()
    }

    def writeDefinition(definition: Definition): Unit =
      definition match {
        case Definition.ConstructorDef(ctor) => writeConstructor(ctor, false)

        case Definition.Enum(name, langName, cases*) =>
          writer.print("enum ")
          writer.print(langName.getOrElse(determineName(name)))
          writer.println(" derives dev.argon.esexpr.ESExprCodec {")
          for enumCase <- cases do
            writeConstructor(enumCase, true)

          writer.println("}")

        case Definition.Const(langName, dataType, value) =>
          writer.print("val ")
          writer.print(langName)
          writer.print(": ")
          writeType(dataType)
          writer.print(" = ")
          writeValue(writer, value)
          writer.println()
      }

    definitions.foreach(writeDefinition)

  end generate

  private def buildNameMap(definition: Definition): Map[String, String] =
    definition match {
      case Definition.ConstructorDef(ctor) => Map(ctor.name -> ctor.langName.getOrElse(determineName(ctor.name)))
      case Definition.Enum(name, langName, cases*) => Map(name -> langName.getOrElse(determineName(name)))
      case Definition.Const(langName, dataType, value) => Map.empty
    }
}
