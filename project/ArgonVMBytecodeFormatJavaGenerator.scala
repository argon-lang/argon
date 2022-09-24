import io.circe.Json
import sbt.io.IO

import java.io.{File, PrintWriter}
import java.nio.charset.StandardCharsets
import scala.util.Using

object ArgonVMBytecodeFormatJavaGenerator {

  def generateFromJSONFile(file: File, outputFile: File): Unit = {
    val jsonText = IO.read(file)
    val json = io.circe.parser.parse(jsonText).right.get

    Using.resource(new PrintWriter(outputFile, StandardCharsets.UTF_8)) { writer =>
      generate(json, writer)
      writer.flush()
    }
  }

  private final case class InstructionInfo(name: String, opcode: Byte, parameters: Seq[InstructionParameter])
  private final case class InstructionParameter(name: String, paramType: OperandType)

  sealed trait OperandType
  object OperandType {
    case object Index extends OperandType
    case object Int16 extends OperandType
  }

  def generate(json: Json, writer: PrintWriter): Unit = {
    writer.println("package dev.argon.argonvm;")
    writer.println("import static dev.argon.argonvm.FormatUtil.*;")
    writer.println("public sealed interface Instruction {")

    writer.println("\tint opcodeWidth();")
    writer.println("\tvoid write(java.io.OutputStream os) throws java.io.IOException;")

    val instructions = json.asObject.get.toIterable
      .map { case (name, options) =>
        options.asNumber match {
          case Some(number) =>
            InstructionInfo(name, number.toByte.get, Seq.empty)

          case None =>
            val obj = options.asObject.get
            InstructionInfo(
              name = name,
              opcode = obj("opcode").get.asNumber.get.toByte.get,
              parameters =
                obj("parameters")
                  .getOrElse(Json.arr())
                  .asArray
                  .get
                  .map { param =>
                    val paramObj = param.asObject.get
                    InstructionParameter(
                      name = paramObj("name").get.asString.get,
                      paramType = paramObj("type").get.asString.get match {
                        case "index" => OperandType.Index
                        case "int16" => OperandType.Int16
                        case t => throw new Exception(s"Invalid operand type: $t")
                      },
                    )
                  }
            )
        }
      }

    instructions.foreach { insn =>
      writer.print(s"\tfinal record ${insn.name}(")
      def outputParam(param: InstructionParameter): Unit = {
        val typeName = param.paramType match {
          case OperandType.Index => "long"
          case OperandType.Int16 => "short"
        }
        writer.print(s"$typeName ${param.name}")
      }
      insn.parameters.headOption.foreach(outputParam)
      insn.parameters.drop(1).foreach { param =>
        writer.print(", ")
        outputParam(param)
      }
      writer.println(") implements Instruction {")
      writer.println("\t\t@Override")
      writer.println("\t\tpublic int opcodeWidth() {")
      writer.println("\t\treturn 1;")
      writer.println("\t\t}")
      writer.println("\t\t@Override")
      writer.println("\t\tpublic void write(java.io.OutputStream os) throws java.io.IOException {")
      writer.println(s"\t\t\tos.write(${insn.opcode});")
      insn.parameters.foreach { param =>
        param.paramType match {
          case OperandType.Index => writer.println(s"\t\t\twriteIndex(os, ${param.name});")
          case OperandType.Int16 => writer.println(s"\t\t\twriteInt16(os, ${param.name});")
        }
      }
      writer.println("\t\t}")
      writer.println("\t}")
    }

    writer.println("\tpublic static Instruction read(java.io.InputStream is) throws java.io.IOException, VMFormatException {")
    writer.println("\t\tbyte opcode = readByte(is);")
    writer.println("\t\tswitch(opcode) {")
    instructions.foreach { insn =>
      writer.println(s"\t\t\tcase ${insn.opcode}:")
      writer.println("\t\t\t{")
      insn.parameters.foreach { param =>
        param.paramType match {
          case OperandType.Index =>
            writer.println(s"\t\t\t\t\tlong ${param.name} = readIndex(is);")

          case OperandType.Int16 =>
            writer.println(s"\t\t\t\t\tshort ${param.name} = readInt16(is);")
        }
      }
      writer.println(s"\t\t\t\treturn new ${insn.name}(${insn.parameters.map(_.name).mkString(", ")});")
      writer.println("\t\t\t}")
    }
    writer.println("\t\t\tdefault: throw new InvalidOpcodeException(opcode);")
    writer.println("\t\t}")
    writer.println("\t}")

    writer.println("}")
  }

}
