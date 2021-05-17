package dev.argon.compiler.lookup

import dev.argon.compiler._
import dev.argon.util.{FileSpec, MemoCache, SourceLocation}
import dev.argon.compiler.core._
import dev.argon.compiler.types.{TypeSystem, TypeSystemConverter}
import cats.implicits._
import dev.argon.compiler.expr._
import zio.IO
import zio.interop.catz.core._

trait ScopeContext[TContext <: Context with Singleton] {

  val context: TContext
  val typeSystem: TypeSystem.Aux[context.type]

  sealed trait Scope {

    def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult]
    def findOperator(op: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult]

    def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): other.Scope = new other.Scope {

      override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[other.LookupResult] =
        Scope.this.findIdentifier(name, fileSpec, sourceLocation)
          .flatMap { _.convertScopeContext(other)(converter) }

      override def findOperator(op: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[other.LookupResult] =
        Scope.this.findOperator(op, fileSpec, sourceLocation)
          .flatMap { _.convertScopeContext(other)(converter) }
    }

  }

  implicit final class ScopeExtensions(val scope: Scope) {

    def addVariable(variable: Variable[context.type, typeSystem.TTypeWrapper]): Scope =
      new Scope {
        override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult] =
          if(variable.name === VariableName.Normal(name)) {
            val value = VariableScopeValue(variable)
            IO.succeed(
              LookupResult.SingleValueResult(value)
            )
          }
          else {
            scope.findIdentifier(name, fileSpec, sourceLocation)
          }

        override def findOperator(op: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult] =
          scope.findOperator(op, fileSpec, sourceLocation)
      }

    def addVariables(variables: Vector[Variable[context.type, typeSystem.TTypeWrapper]]): Scope =
      variables.foldLeft(scope) { (scope, variable) => scope.addVariable(variable) }

    def addParameter(param: Parameter[context.type, typeSystem.TTypeWrapper]): Scope = {
      def addParamElement(scope: Scope, paramElem: ParameterElement[context.type, typeSystem.TTypeWrapper]): Scope =
        new Scope {
          override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult] =
            if(paramElem.name === VariableName.Normal(name)) {
              val value = ParameterElementScopeValue(paramElem)
              IO.succeed(
                LookupResult.SingleValueResult(value)
              )
            }
            else {
              scope.findIdentifier(name, fileSpec, sourceLocation)
            }

          override def findOperator(op: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult] =
            scope.findOperator(op, fileSpec, sourceLocation)
        }

      if(param.elements.size > 1)
        param.elements.foldLeft(scope) { (scope, variable) => addParamElement(scope, variable) }
      else
        addVariable(param.paramVar)
    }

    def addParameters(params: Vector[Parameter[context.type, typeSystem.TTypeWrapper]]): Scope =
      params.foldLeft(scope) { (scope, param) => scope.addParameter(param) }

  }

  sealed trait NamespacesOnlyScope extends Scope

  case object EmptyScope extends NamespacesOnlyScope {
    override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult] =
      IO.succeed(LookupResult.Failed)

    override def findOperator(op: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult] =
      IO.succeed(LookupResult.Failed)
  }

  final class NamespaceScope private(findId: MemoCache[Any, CompilationError, (GlobalName.NonEmpty, FileSpec, SourceLocation), LookupResult], parentScope: NamespacesOnlyScope) extends NamespacesOnlyScope {

    override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult] =
      findId.get((GlobalName.Normal(name), fileSpec, sourceLocation))

    override def findOperator(op: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult] =
      findId.get((GlobalName.Operator(op), fileSpec, sourceLocation))
  }

  object NamespaceScope {

    def apply(findId: (GlobalName.NonEmpty, FileSpec, SourceLocation) => Comp[LookupResult], parentScope: NamespacesOnlyScope): Comp[NamespaceScope] =
      MemoCache.make(findId.tupled).map { findIdMemo =>
        new NamespaceScope(findIdMemo, parentScope)
      }

  }


  sealed trait ScopeValue {
    def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): Comp[other.ScopeValue]
  }
  sealed trait ScopeValueSingle extends ScopeValue {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): Comp[other.ScopeValueSingle]
  }
  sealed trait ScopeValueOverload extends ScopeValue {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): Comp[other.ScopeValueOverload]
  }
  final case class VariableScopeValue(variable: Variable[context.type, typeSystem.TTypeWrapper]) extends ScopeValueSingle {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): Comp[other.ScopeValueSingle] =
      converter.convertVariableTypeSystem(variable).map(other.VariableScopeValue)
  }
  final case class ParameterElementScopeValue(paramElem: ParameterElement[context.type, typeSystem.TTypeWrapper]) extends ScopeValueSingle {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): Comp[other.ScopeValueSingle] =
      converter.convertParameterElementTypeSystem(paramElem).map(other.ParameterElementScopeValue)
  }
  final case class FunctionScopeValue(func: AbsRef[context.type, ArFunc]) extends ScopeValueOverload {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): Comp[other.ScopeValueOverload] =
      other.FunctionScopeValue(func).pure[Comp]
  }
  final case class TraitScopeValue(arTrait: AbsRef[context.type, ArTrait]) extends ScopeValueOverload {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): Comp[other.ScopeValueOverload] =
      other.TraitScopeValue(arTrait).pure[Comp]
  }
  final case class ClassScopeValue(arClass: AbsRef[context.type, ArClass]) extends ScopeValueOverload {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): Comp[other.ScopeValueOverload] =
      other.ClassScopeValue(arClass).pure[Comp]
  }
  final case class DataConstructorScopeValue(ctor: AbsRef[context.type, DataConstructor]) extends ScopeValueOverload {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): Comp[other.ScopeValueOverload] =
      other.DataConstructorScopeValue(ctor).pure[Comp]
  }

  sealed trait LookupResult {
    def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): Comp[other.LookupResult]
  }
  object LookupResult {
    final case class ScopeResult(scope: Scope) extends LookupResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): Comp[other.LookupResult] =
        other.LookupResult.ScopeResult(scope.convertScopeContext(other)(converter)).pure[Comp]
    }
    final case class SingleValueResult(value: ScopeValueSingle) extends LookupResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): Comp[other.LookupResult] =
        value.convertScopeContext(other)(converter).map(other.LookupResult.SingleValueResult)
    }
    final case class ValuesResult(overloads: OverloadResult.List[ScopeValueOverload]) extends LookupResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): Comp[other.LookupResult] =
        overloads.traverse { _.convertScopeContext(other)(converter) }.map(other.LookupResult.ValuesResult)
    }
    case object Failed extends LookupResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.TTypeWrapper, other.typeSystem.TTypeWrapper]): Comp[other.LookupResult] =
        other.LookupResult.Failed.pure[Comp]
    }
  }

}

