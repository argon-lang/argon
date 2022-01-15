package dev.argon.compiler

import dev.argon.compiler.module.ModuleName

trait DiagnosticMessage
trait DiagnosticError
trait DiagnosticSource


object DiagnosticError {
  final case class UnknownModuleException(moduleName: ModuleName) extends DiagnosticError
  final case class InvalidTopLevelStatement(stmt: dev.argon.parser.Stmt) extends DiagnosticError
  final case class InvalidTypeForFunction() extends DiagnosticError
  final case class InvalidTypeForTuple() extends DiagnosticError
  final case class UnknownTypeForExpression() extends DiagnosticError
  final case class TupleSizeMismatch() extends DiagnosticError
  final case class InvalidStatementInFunction() extends DiagnosticError
  final case class LookupFailed() extends DiagnosticError
  final case class AmbiguousOverload() extends DiagnosticError
  final case class CanNotMutate() extends DiagnosticError
  final case class TypeError() extends DiagnosticError
}

