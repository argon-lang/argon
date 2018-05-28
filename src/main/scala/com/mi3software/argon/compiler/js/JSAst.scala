package com.mi3software.argon.compiler.js

import java.io.PrintWriter

import org.apache.commons.lang3.StringEscapeUtils

final case class JSIdentifier(id: String) extends AnyVal

final case class JSModule(statements: Vector[JSModuleStatement])

sealed trait JSModuleStatement

sealed trait JSExportStatement extends JSModuleStatement
final case class JSExportDefaultStatement(expr: JSExpression) extends JSExportStatement

sealed trait JSImportStatement extends JSModuleStatement
final case class JSImportDefaultStatement(defaultExport: JSIdentifier, moduleName: String) extends JSImportStatement

sealed trait JSStatement extends JSModuleStatement


sealed trait JSExpression extends JSStatement

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
          writeExpr(expr)
          writer.print(";")

        case JSImportDefaultStatement(defaultExport, moduleName) =>
          writer.print("import ")
          writeIdentifier(defaultExport)
          writer.print(" from ")
          writeString(moduleName)
          writer.print(";")
      }

    def writeExpr(expr: JSExpression): Unit =
      ???

    def writeIdentifier(identifier: JSIdentifier): Unit =
      writer.print(identifier.id)

    def writeString(str: String): Unit = {
      writer.print("\"")
      StringEscapeUtils.ESCAPE_ECMASCRIPT.translate(str, writer)
      writer.print("\"")
    }

  }

}
