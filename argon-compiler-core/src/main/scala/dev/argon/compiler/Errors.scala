package dev.argon.compiler

import dev.argon.compiler.module.ModuleName
import dev.argon.compiler.tube.TubeName
import dev.argon.compiler.definitions.*

trait DiagnosticMessage
trait DiagnosticError
trait DiagnosticSource

object DiagnosticError {
  final case class UnknownTube(tubeName: TubeName) extends DiagnosticError
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
  final case class InvalidAccessModifierCombination() extends DiagnosticError
  final case class InvalidGlobalAccessModifier(accessModifier: AccessModifier) extends DiagnosticError

  final case class SpecMultiPartGlobUsedWithPrefixSuffix() extends DiagnosticError
  final case class SpecFileNameTemplateMustBeIdentifier() extends DiagnosticError
  final case class SpecOneVariablePerTemplateSegment() extends DiagnosticError
  final case class SpecEmptyTemplateSegment() extends DiagnosticError
  final case class SpecUndefinedVariable() extends DiagnosticError
}
