package dev.argon.esexpr.generator

import dev.argon.esexpr.schema.*
import dev.argon.esexpr.schema.Definition.SimpleEnum
import dev.argon.esexpr.ESExprCodec
import dev.argon.esexpr.parser.ESExprTextReader
import dev.argon.util.{*, given}
import org.apache.commons.lang3.StringUtils
import org.apache.commons.text.StringEscapeUtils
import zio.*
import zio.stream.*

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

private[generator] final class JavaGenerator
(
  val writer: PrintWriter,
  val definitions: Seq[Definition],
  packageName: String
) extends GeneratorBase with JavaGeneratorNaming with JavaGeneratorUtil {


  override def writePackageQualifier(): Unit =
    w(packageName)
    w(".")
  end writePackageQualifier


  def writeDefinition(definition: Definition): Unit =
    w("package ")
    w(packageName)
    wl(";")

    definition match {
      case Definition.ConstructorDef(ctor) => writeConstructor(ctor, None)

      case definition @ Definition.Enum(name, cases*) =>
        writeInterface(name, `sealed` = true) {
          for enumCase <- cases do
            writeConstructor(enumCase, Some(definition))
        }

      case Definition.Const(name, dataType, value) =>
        writeClass(name) {
          w("public static final ")
          writeType(dataType)
          w(determineConstantName(name))
          w(" = ")
          writeValue(value)
          w(";")
        }

      case Definition.SimpleEnum(name, values*) =>
        w("public enum ")
        writeTypeName(name)
        wl(" {")
        indent {
          for value <- values do
            w(getEnumCaseName(value))
            wl(",")
          end for
        }
        wl("}")

      case Definition.EnumClass(name, typeParameters, cases*) =>
        w("public sealed interface ")
        writeTypeName(name)
        w("<E extends java.lang.Throwable")
        for tp <- typeParameters do
          w(", ")
          writeTypeParameter(tp)
        end for
        wl("> {")

        indent {
          for enumCase <- cases do
            w("non-sealed interface ")
            writeTypeName(enumCase.name)
            w("<E extends java.lang.Throwable")
            for tp <- typeParameters do
              w(", ")
              writeTypeParameter(tp)
            end for
            w("> extends ")
            writeTypeName(name)
            w("<E")
            for tp <- typeParameters do
              w(", ")
              writeTypeName(tp.name)
            end for
            wl("> {")
            indent {
              enumCase.members.foreach(writeMember)
            }
            wl("}")
          end for
        }
        wl("}")

      case Definition.Interface(name, typeParameters, extendedTypes, members*) =>
        w("public interface ")
        writeTypeName(name)
        w("<E extends java.lang.Throwable")
        for tp <- typeParameters do
          w(", ")
          writeTypeParameter(tp)
        end for
        w(">")
        if extendedTypes.nonEmpty then
          w(" extends ")
          writeCommaListSingleLine(writeBaseType)(extendedTypes)
        end if
        wl(" {")
        indent {
          members.foreach(writeMember)
        }
        wl("}")

      case Definition.TypeStruct(name, values*) =>
        writeInterface(name, `sealed` = true, typeParameters = Seq(TypeParameter("T", tuple = Some(name), `enum` = None))) {

          def writeTypeParams(): Unit =
            writeCommaListSingleLine[String](t => {
              w("T")
              writeTypeName(t)
            })(values)

          w("non-sealed interface Impl<")
          writeTypeParams()
          w("> extends ")
          writeTypeName(name)
          w("<Impl<")
          writeTypeParams()
          wl(">> {}")

          for value <- values do
            writeInterface(
              value,
              `sealed` = true,
              typeParameters = Seq(TypeParameter("T", tuple = Some(name), `enum` = None)),
            ) {}

            w("record ")
            writeTypeName(value)
            w("Value<")
            writeTypeParams()
            w(">(T")
            writeTypeName(value)
            w(" ")
            writeValueName(value)
            w(") implements ")
            writeTypeName(value)
            w("<Impl<")
            writeTypeParams()
            wl(">> {}")

            w("static <")
            writeTypeParams()
            w("> T")
            writeTypeName(value)
            w(" get")
            writeTypeName(value)
            w("(")
            writeTypeName(value)
            w("<Impl<")
            writeTypeParams()
            w(">> ")
            writeValueName(value)
            wl(") {")
            indent {
              w("return switch(")
              writeValueName(value)
              wl(") {")
              indent {
                w("case ")
                writeTypeName(value)
                w("Value<")
                writeTypeParams()
                w(s"> $reservedValueName -> $reservedValueName.")
                writeValueName(value)
                wl("();")
              }
              wl("};")
            }
            wl("}")


          end for
        }

      case _: Definition.Extern => throw new UnsupportedOperationException()
    }
  end writeDefinition

  def writeConstructor(ctor: Constructor, owningEnum: Option[Definition.Enum]): Unit =
    writeRecord(ctor.name, `implements` = owningEnum.map(e => DataType.UserDefined(e.name)).toSeq)(
      ctor.parameters*
    ) {}

  def writeMember(m: Member): Unit =
    m match {
      case Member.Field(name, fieldType) =>
        writeType(fieldType)
        w(" ")
        writeValueName(name)
        wl("();")

      case Member.Method(name, _, async, params, returnType) =>
        writeType(returnType)
        w(" ")
        writeValueName(name)
        w("(")
        writeCommaListSingleLine(writeMethodParameter)(params)
        w(")")
        if async then
          w(" throws E, java.io.IOException, java.lang.InterruptedException")
        end if
        wl(";")
    }

}

object JavaGenerator {

  def generate(outDir: Path, definitions: Seq[Definition], config: GeneratorConfig.Java): Task[Unit] =
    ZIO.foreachDiscard(definitions) {
      case _: Definition.Extern => ZIO.unit
      case definition =>
        ZIO.attempt {
          val fileName = GeneratorBase.determineNamePascalCase(GeneratorUtil.getDefinitionName(definition))
          val outFile = outDir.resolve(config.outDir).nn.resolve(fileName + ".java").nn
          Files.createDirectories(outFile.getParent.nn)
          
          Using.resource(Files.newBufferedWriter(outFile).nn) { writer =>
            Using.resource(new PrintWriter(writer)) { writer =>
              val generator = JavaGenerator(writer, definitions, config.packageName)
              generator.writeDefinition(definition)
            }
          }
        }
    }
}
