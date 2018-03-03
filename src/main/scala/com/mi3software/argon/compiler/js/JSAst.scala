package com.mi3software.argon.compiler.js

final case class JSIdentifier(id: String) extends AnyVal

final case class JSModule(statements: Vector[JSModuleStatement])

sealed trait JSModuleStatement

sealed trait JSExportStatement extends JSModuleStatement
final case class JSExportDefaultStatement(expr: JSExpression) extends JSExportStatement

sealed trait JSImportStatement extends JSModuleStatement
final case class JSImportDefaultStatement(defaultExport: JSIdentifier, moduleName: String) extends JSImportStatement

sealed trait JSStatement extends JSModuleStatement


sealed trait JSExpression extends JSStatement

