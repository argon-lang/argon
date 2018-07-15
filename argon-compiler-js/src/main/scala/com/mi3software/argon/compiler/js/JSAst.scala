package com.mi3software.argon.compiler.js

import java.io.PrintWriter

import org.apache.commons.lang3.StringEscapeUtils
import scalaz.NonEmptyList

final case class JSModule(statements: Vector[JSModuleStatement])

sealed trait JSModuleStatement

sealed trait JSExportStatement extends JSModuleStatement
final case class JSExportDefaultStatement(expr: JSExpression) extends JSExportStatement

sealed trait JSImportStatement extends JSModuleStatement
final case class JSImportDefaultStatement(defaultExport: JSIdentifier, moduleName: String) extends JSImportStatement
final case class JSImportAllStatement(defaultExport: Option[JSIdentifier], name: JSIdentifier, moduleName: String) extends JSImportStatement

sealed trait JSStatement extends JSModuleStatement


final case class JSConst(bindings: NonEmptyList[JSBinding]) extends JSStatement

sealed trait JSBinding
final case class JSBindNewVariable(name: String) extends JSBinding
final case class JSBindValue(name: String, value: JSExpression) extends JSBinding



sealed trait JSExpression extends JSStatement

final case class JSObjectLiteral(members: Vector[JSObjectMember]) extends JSExpression

sealed trait JSObjectMember
final case class JSObjectProperty(name: String, value: JSExpression) extends JSObjectMember

final case class JSIdentifier(id: String) extends JSExpression

object JSAst {

  def writeModule(module: JSModule)(writer: PrintWriter): Unit =
    new WriteImpl(writer).writeModule(module)

  private class WriteImpl(writer: PrintWriter) {

    def writeModule(module: JSModule): Unit =
      module.statements foreach writeModuleStatement

    def writeModuleStatement(stmt: JSModuleStatement): Unit =
      stmt match {
        case JSExportDefaultStatement(expr) =>
          writer.print("export default")
          writeExprParen(expr)
          writer.print(";")

        case JSImportDefaultStatement(defaultExport, moduleName) =>
          writer.print("import ")
          writeIdentifier(defaultExport)
          writer.print(" from ")
          writeString(moduleName)
          writer.print(";")

        case JSImportAllStatement(defaultExport, name, moduleName) =>
          writer.print("import ")
          defaultExport.foreach { defaultId =>
            writeIdentifier(defaultId)
            writer.print(", ")
          }
          writer.print("* as ")
          writeIdentifier(name)
          writer.print(" from ")
          writeString(moduleName)
          writer.print(";")

        case stmt: JSStatement =>
          writeStatement(stmt)
      }

    def writeStatement(stmt: JSStatement): Unit =
      stmt match {
        case JSConst(bindings) =>
          writer.print("const ")
          writeBinding(bindings.head)

          for(binding <- bindings.tail.toVector) {
            writer.print(", ")
            writeBinding(binding)
          }
          writer.print(";")

        case stmt: JSExpression =>
          writeExprParen(stmt)
          writer.print(";")
      }

    def writeBinding(binding: JSBinding): Unit =
      binding match {
        case JSBindNewVariable(name) =>
          writer.print(name)

        case JSBindValue(name, value) =>
          writer.print(name)
          writer.print(" = ")
          writeExprParen(value)
      }

    def writeExprParen(expr: JSExpression): Unit = {
      writer.print("(")
      writeExpr(expr)
      writer.print(")")
    }

    def writeExpr(expr: JSExpression): Unit =
      expr match {
        case JSObjectLiteral(members) =>
          writer.print("{")
          writer.print("}")

        case expr: JSIdentifier =>
          writeIdentifier(expr)
      }

    def writeIdentifier(identifier: JSIdentifier): Unit =
      writer.print(identifier.id)

    def writeString(str: String): Unit = {
      writer.print("\"")
      StringEscapeUtils.ESCAPE_ECMASCRIPT.translate(str, writer)
      writer.print("\"")
    }

  }

}
