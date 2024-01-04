package dev.argon.esexpr.generator

import dev.argon.esexpr.schema.{DataType, *}
import dev.argon.esexpr.{ESExpr, ESExprCodec}
import dev.argon.util.{*, given}
import org.apache.commons.io.FilenameUtils
import org.apache.commons.lang3.StringUtils
import org.apache.commons.text.StringEscapeUtils
import zio.{Scope, ZIO}

import java.io.PrintWriter
import java.math.BigInteger
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using


private[generator] final class TypeScriptGenerator
(
  val writer: PrintWriter,
  val definitions: Seq[Definition],
  val externsFileName: String,
) extends GeneratorBase with TypeScriptGeneratorNaming {
  
  def writeInterface(name: String, `extends`: Seq[DataType | String] = Seq(), typeParameters: Seq[TypeParameter] = Seq())(body: => Unit): Unit =
    w("export interface ")
    writeTypeName(name)
    if typeParameters.nonEmpty then
      w("<")
      writeCommaListSingleLine(writeTypeParameter)(typeParameters)
      w(">")
    end if
    
    if `extends`.nonEmpty then
      w("extends ")
      writeCommaListSingleLine(writeBaseTypeOrString)(`extends`)
    end if
    
    wl(" {")
    
    indent {
      body
    }
    
    wl("}")
  end writeInterface

  def writeUnion[A](name: String, typeParameters: Seq[TypeParameter] = Seq())(items: Seq[A])(writeItem: A => Unit): Unit =
    w("export type ")
    writeTypeName(name)
    if typeParameters.nonEmpty then
      w("<")
      writeCommaListSingleLine(writeTypeParameter)(typeParameters)
      w(">")
    end if
    wl(" =")

    indent {
      for (item, i) <- items.zipWithIndex do
        w("| ")
        writeItem(item)

        if i == items.size - 1 then
          wl(";")
        else
          wl("")
      end for
    }
  end writeUnion

  def writeNamespace(name: String)(body: => Unit): Unit =
    w("export namespace ")
    writeTypeName(name)

    wl(" {")

    indent {
      body
    }

    wl("}")
  end writeNamespace

  def generate(): Unit =
    definitions.foreach { d =>
      writeDefinition(d)
      wl("")
    }

  def writeConstructor(ctor: Constructor, isCase: Boolean): Unit = {
    writeInterface(ctor.name) {

      if isCase then
        w("type: \"")
        w(StringEscapeUtils.escapeEcmaScript(ctor.name).nn)
        wl("\";")
      end if

      ctor.parameters.foreach(writeParameter)
      
    }
  }

  def writeConstructorCodec(ctor: Constructor, isCase: Boolean): Unit = {
    writeNamespace(ctor.name) {
      w("const codec: ESExpr.ESExprCodec<")
      writeTypeName(ctor.name)
      wl("> = {")
      indent {
        w("def ")
      }
      wl("}")
    }
  }

  def writeParameter(p: Parameter): Unit =
    p match {
      case Parameter.KeywordParam(name, paramType, optional, _) =>
        w("readonly ")
        writeValueName(name)
        w(": ")
        writeType(paramType)
        if optional then
          w(" | undefined")
        wl(";")

      case Parameter.Dict(name, paramType) =>
        w("readonly ")
        writeValueName(name)
        w(": ReadonlyMap<string, ")
        writeType(paramType)
        w(">")

      case Parameter.PositionalParam(name, paramType) =>
        w("readonly ")
        writeValueName(name)
        w(": ")
        writeType(paramType)
        wl(";")

      case Parameter.VarArgParam(name, paramType) =>
        w("readonly ")
        writeValueName(name)
        w(": readonly ")
        writeType(paramType)
        wl("[];")
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
      case Definition.ConstructorDef(ctor) => writeConstructor(ctor, false)

      case Definition.Enum(name, cases*) =>
        writeUnion(name)(cases) { enumCase =>
          writeTypeName(name)
          w(".")
          writeTypeName(enumCase.name)
        }

        writeNamespace(name) {
          for enumCase <- cases do
            writeConstructor(enumCase, true)
        }

      case Definition.Const(name, dataType, value) =>
        w("export const ")
        writeValueName(name)
        w(": ")
        writeType(dataType)
        w(" = ")
        writeValue(value, dataType)
        wl("")

      case Definition.SimpleEnum(name, values*) =>
        writeUnion(name)(values)(writeString)

      case Definition.EnumClass(name, typeParameters, cases*) =>
        writeUnion(name, typeParameters = typeParameters)(cases) { enumCase =>
          wl("{")
          indent {
            w("type: ")
            writeString(enumCase.name)
            wl(";")

            enumCase.members.foreach(writeMember)
          }
          w("}")
        }

      case Definition.Interface(name, typeParameters, extendedTypes, members*) =>
        writeInterface(name, typeParameters = typeParameters, `extends` = extendedTypes) {
          members.foreach(writeMember)
        }

      case Definition.TypeEnum(name, values*) =>
        writeUnion(name)(values)(writeString)

      case Definition.TypeStruct(name, values*) =>
        writeInterface(name) {
          for value <- values do
            writeString(value)
            wl(": unknown;")
          end for
        }

      case _: Definition.Extern =>
    }

  override def writeType(t: DataType): Unit = t match {
    case DataType.UserDefined(name) =>
      if definitions.exists { case Definition.Extern(n, _) => n == name case _ => false } then
        w("import(\"./")
        w(externsFileName)
        w("\").")
      end if
      writeTypeName(name)

    case DataType.Apply(name, args*) =>
      if definitions.exists { case Definition.Extern(n, _) => n == name case _ => false } then
        w("import(\"./")
        w(externsFileName)
        w("\").")
      end if
      writeTypeName(name)
      w("<")
      writeCommaListSingleLine(writeType)(args)
      w(">")

    case DataType.List(elementType) =>
      val paren = typeNeedsParen(elementType)
      w("readonly ")
      if paren then w("(")
      writeType(elementType)
      if paren then w(")")
      w("[]")

    case DataType.Set(elementType) =>
      w("ReadonlySet<")
      writeType(elementType)
      w(">")

    case DataType.Dict(elementType) =>
      w("ReadonlyMap<string, ")
      writeType(elementType)
      w(">")

    case DataType.TypeEnumMatch(_, t, cases) =>
      def writeCases(cases: List[(String, DataType)]): Unit =
        cases match {
          case (caseName, mappedType) :: tail =>
            writeType(t)
            w(" extends ")
            writeString(caseName)
            w(" ? (")
            writeType(mappedType)
            w(") : (")
            writeCases(tail)
            w(")")

          case Nil =>
            w("never")
        }

      writeCases(cases.toList)

    case DataType.TypeStructMember(_, t, member) =>
      writeType(t)
      w("[")
      writeString(member)
      w("]")

    case DataType.Nullable(t) =>
      val paren = typeNeedsParen(t)
      if paren then w("(")
      writeType(t)
      if paren then w(")")
      w(" | null")

    case DataType.Bool => w("boolean")
    case DataType.IntType => w("bigint")
    case DataType.UInt32 | DataType.Int32 => w("number")
    case DataType.UInt64 | DataType.Int64 => w("bigint")
    case DataType.Str => w("string")
    case DataType.Binary => w("Uint8Array")
    case DataType.Float32 => w("number")
    case DataType.Float64 => w("number")
    case DataType.ESExpr => w("import(\"@argon-lang/esexpr\").ESExpr")
    case DataType.ESExprTag => w("import(\"@argon-lang/esexpr\").ESExprTag")
  }

  def typeNeedsParen(t: DataType): Boolean =
    t match {
      case DataType.List(_) | DataType.Nullable(_) | _: DataType.TypeEnumMatch => true
      case _ => false
    }

  def writeTypeParameter(p: TypeParameter): Unit =
    writeTypeName(p.name)
    p.tuple.orElse(p.`enum`).foreach { t =>
      w(" extends ")
      writeTypeName(t)
    }
  end writeTypeParameter

  def writeMember(m: Member): Unit =
    m match {
      case Member.Field(name, fieldType) =>
        w("readonly ")
        writeValueName(name)
        w(": ")
        writeType(fieldType)
        wl(";")

      case Member.Method(name, _, async, params, returnType) =>
        writeValueName(name)
        w("(")
        writeCommaListSingleLine(writeMethodParameter)(params)
        w("): ")
        if async then w("Promise<")
        writeType(returnType);
        if async then w(">")
        wl(";")
    }

  def writeInt(t: DataType, value: BigInt): Unit =
    t match {
      case DataType.UInt32 | DataType.Int32 =>
        w(value.toString)

      case _ =>
        w(value.toString())
        w("n")
    }

  def writeString(s: String): Unit =
    w("\"")
    w(StringEscapeUtils.escapeEcmaScript(s).nn)
    w("\"")
  end writeString

  def writeValue(value: ESExpr, t: DataType): Unit =
    value match {
      case ESExpr.Constructed(constructor, kwargs, args) => ???
      case ESExpr.Bool(b) => w(b.toString)
      case ESExpr.Int(n) => writeInt(t, n)
      case ESExpr.Str(s) => writeString(s)

      case ESExpr.Binary(b) =>
        w("new Uint8Array([")
        for i <- b.indices do
          w(b(i).toString)
          if i < b.length - 1 then w(", ")
        end for
        w("])")

      case ESExpr.Float32(f) =>
        writeNumber(f)

      case ESExpr.Float64(d) =>
        writeNumber(d)

      case ESExpr.Null => w("null")
    }

  private def writeNumber(d: Double): Unit =
    if (d.isNaN) w("Number.NaN")
    else if (d == Double.PositiveInfinity) w("Number.POSITIVE_INFINITY")
    else if (d == Double.NegativeInfinity) w("Number.NEGATIVE_INFINITY")
    else w(d.toString)

}

object TypeScriptGenerator {

  def generate(outDir: Path, definitions: Seq[Definition], config: GeneratorConfig.TypeScript): ZIO[Scope, Any, Unit] =
    ZIO.attempt {
      val outFile = outDir.resolve(config.outFile).nn
      Files.createDirectories(outFile.getParent.nn)
      val externsFileName = FilenameUtils.getBaseName(config.outFile).nn + ".extern.js"

      Using.resource(Files.newBufferedWriter(outFile).nn) { writer =>
        Using.resource(new PrintWriter(writer)) { writer =>
          TypeScriptGenerator(writer, definitions, externsFileName).generate()
        }
      }
    }

}
