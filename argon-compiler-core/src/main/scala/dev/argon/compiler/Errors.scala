package dev.argon.compiler

import dev.argon.compiler.module.ModuleName
import dev.argon.compiler.tube.TubeName
import dev.argon.compiler.definitions.*
import dev.argon.parser
import dev.argon.parser.IdentifierExpr
import dev.argon.util.{SourceLocation, TypeNameTag}

trait DiagnosticMessage
sealed trait DiagnosticError derives TypeNameTag

object DiagnosticError {
  final case class UnknownTube(tubeName: TubeName) extends DiagnosticError
  final case class UnknownModuleException(moduleName: ModuleName) extends DiagnosticError
  final case class InvalidTopLevelStatement(stmt: dev.argon.parser.Stmt) extends DiagnosticError
  final case class InvalidTypeForFunction() extends DiagnosticError
  final case class InvalidTypeForTuple() extends DiagnosticError
  final case class UnknownTypeForExpression() extends DiagnosticError
  final case class TupleSizeMismatch() extends DiagnosticError
  final case class InvalidStatementInFunction() extends DiagnosticError
  final case class LookupFailed(source: DiagnosticSource, lookupName: IdentifierExpr | parser.ClassConstructorExpr.type) extends DiagnosticError
  final case class OverloadsFailed(source: DiagnosticSource, lookupName: IdentifierExpr | parser.ClassConstructorExpr.type) extends DiagnosticError
  final case class AmbiguousOverload() extends DiagnosticError
  final case class CanNotMutate(source: DiagnosticSource) extends DiagnosticError
  final case class TypeError(source: DiagnosticSource) extends DiagnosticError
  final case class InvalidAccessModifierCombination() extends DiagnosticError
  final case class InvalidGlobalAccessModifier(accessModifier: AccessModifier) extends DiagnosticError
  final case class ExternMethodNotFound(source: DiagnosticSource, specifier: String) extends DiagnosticError
  final case class ExternFunctionNotFound(source: DiagnosticSource, specifier: String) extends DiagnosticError
  final case class FieldNotFound(source: DiagnosticSource, name: IdentifierExpr) extends DiagnosticError
  final case class Purity(source: DiagnosticSource) extends DiagnosticError
  final case class NonOpenClassExtended(source: DiagnosticSource) extends DiagnosticError
  final case class SealedClassExtended(source: DiagnosticSource) extends DiagnosticError
  final case class SealedTraitExtended(source: DiagnosticSource) extends DiagnosticError
  final case class AbstractClassConstructorCalled(source: DiagnosticSource) extends DiagnosticError
  final case class AbstractMethodNotImplemented(source: DiagnosticSource, name: Option[IdentifierExpr]) extends DiagnosticError
  final case class FieldNotInitialized(source: DiagnosticSource) extends DiagnosticError
  final case class FieldReinitialized(source: DiagnosticSource) extends DiagnosticError
  final case class ErasedExpressionNotAllowed(source: DiagnosticSource) extends DiagnosticError
  final case class ImplicitNotFound(source: DiagnosticSource) extends DiagnosticError
  final case class ProofMustBePure(source: DiagnosticSource) extends DiagnosticError
  final case class ErasedMustBePure(source: DiagnosticSource) extends DiagnosticError


  final case class SpecMultiPartGlobUsedWithPrefixSuffix() extends DiagnosticError
  final case class SpecFileNameTemplateMustBeIdentifier() extends DiagnosticError
  final case class SpecOneVariablePerTemplateSegment() extends DiagnosticError
  final case class SpecEmptyTemplateSegment() extends DiagnosticError
  final case class SpecUndefinedVariable() extends DiagnosticError
}

enum DiagnosticSource {
  case Location(location: SourceLocation)
  case SerializedTube(tubeName: TubeName)
}
