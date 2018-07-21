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
final case class JSFunctionStatement(name: JSIdentifier, parameters: JSFunctionParameterList, body: Vector[JSStatement]) extends JSStatement

sealed trait JSBinding
final case class JSBindNewVariable(name: JSIdentifier) extends JSBinding
final case class JSBindValue(name: JSIdentifier, value: JSExpression) extends JSBinding

sealed trait JSFunctionParameterList
case object JSFunctionEmptyParameterList extends JSFunctionParameterList
final case class JSFunctionParameter(name: JSIdentifier, next: JSFunctionParameterList) extends JSFunctionParameterList
final case class JSFunctionRestParameters(name: JSIdentifier) extends JSFunctionParameterList

sealed trait JSExpression extends JSStatement

final case class JSObjectLiteral(members: Vector[JSObjectMember]) extends JSExpression

sealed trait JSObjectMember
final case class JSObjectProperty(name: String, value: JSExpression) extends JSObjectMember

final case class JSIdentifier(id: String) extends JSExpression

final case class JSAssignment(left: JSExpression, right: JSExpression) extends JSExpression
final case class JSPropertyAccessDot(expr: JSExpression, prop: JSIdentifier) extends JSExpression
final case class JSPropertyAccessBracket(expr: JSExpression, prop: JSExpression) extends JSExpression
final case class JSString(value: String) extends JSExpression
final case class JSFunctionCall(function: JSExpression, args: Vector[JSExpression]) extends JSExpression

case object JSNull extends JSExpression

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

        case JSFunctionStatement(name, parameters, body) =>
          writer.print("function ")
          writeIdentifier(name)
          writer.print("(")
          writeParameterList(parameters)
          writer.print("){")
          body.foreach(writeStatement)
          writer.print("}")

        case stmt: JSExpression =>
          writeExprParen(stmt)
          writer.print(";")
      }

    def writeBinding(binding: JSBinding): Unit =
      binding match {
        case JSBindNewVariable(name) =>
          writeIdentifier(name)

        case JSBindValue(name, value) =>
          writeIdentifier(name)
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
          members.foreach {
            case JSObjectProperty(name, value) =>
              writeString(name)
              writer.print(":")
              writeExprParen(value)
              writer.print(",")
          }
          writer.print("}")

        case expr: JSIdentifier =>
          writeIdentifier(expr)

        case JSAssignment(left, right) =>
          writeExprParen(left)
          writer.print("=")
          writeExprParen(right)

        case JSPropertyAccessDot(expr, prop) =>
          writeExprParen(expr)
          writer.print(".")
          writeIdentifier(prop)

        case JSPropertyAccessBracket(expr, prop) =>
          writeExprParen(expr)
          writer.print("[")
          writeExpr(prop)
          writer.print("]")

        case JSString(value) =>
          writeString(value)

        case JSFunctionCall(function, args) =>
          writeExprParen(function)
          writer.print("(")
          for(arg <- args) {
            writeExprParen(arg)
            writer.print(",")
          }
          writer.print(")")

        case JSNull =>
          writer.print("null")
      }

    def writeIdentifier(identifier: JSIdentifier): Unit =
      writer.print(identifier.id)

    def writeString(str: String): Unit = {
      writer.print("\"")
      StringEscapeUtils.ESCAPE_ECMASCRIPT.translate(str, writer)
      writer.print("\"")
    }

    def writeParameterList(params: JSFunctionParameterList): Unit =
      params match {
        case JSFunctionEmptyParameterList => ()
        case JSFunctionParameter(name, next) =>
          writeIdentifier(name)
          writer.print(",")
          writeParameterList(next)

        case JSFunctionRestParameters(name) =>
          writer.print("...")
          writeIdentifier(name)
      }

  }

}
