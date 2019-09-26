package dev.argon.compiler.lookup

import dev.argon.compiler._
import dev.argon.util.{FileSpec, NamespacePath, SourceLocation}
import dev.argon.compiler.core._
import dev.argon.compiler.types.{TypeSystem, TypeSystemConverter}
import cats._
import cats.implicits._

trait ScopeContext[TContext <: Context with Singleton] {

  val context: TContext
  val typeSystem: TypeSystem[context.type]

  import context.{ Comp, compCompilationInstance }
  import typeSystem.{Variable, ParameterElement, Parameter}

  sealed trait Scope {

    def nextVariable: Int

    def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult]

    def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): other.Scope = new other.Scope {

      override def nextVariable: Int = Scope.this.nextVariable

      override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[other.LookupResult] =
        Scope.this.findIdentifier(name, fileSpec, sourceLocation)
          .flatMap { _.convertScopeContext(other)(converter) }
      
    }

  }

  implicit final class ScopeExtensions(val scope: Scope) {

    def addVariable(variable: Variable): Scope =
      new Scope {
        override def nextVariable: Int = variable.descriptor match {
          case VariableDescriptor(_, id) => id + 1
          case _ => scope.nextVariable
        }

        override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult] =
          if(variable.name === VariableName.Normal(name)) {
            val value = VariableScopeValue(variable)
            context.compCompilationInstance.point(
              LookupResult.SingleValueResult(value)
            )
          }
          else {
            scope.findIdentifier(name, fileSpec, sourceLocation)
          }

      }

    def addVariables(variables: Vector[Variable]): Scope =
      variables.foldLeft(scope) { (scope, variable) => scope.addVariable(variable) }

    def addParameter(param: Parameter): Scope = {
      def addParamElement(scope: Scope, paramElem: ParameterElement): Scope =
        new Scope {
          override def nextVariable: Int = scope.nextVariable

          override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult] =
            if(paramElem.name === VariableName.Normal(name)) {
              val value = ParameterElementScopeValue(paramElem)
              context.compCompilationInstance.point(
                LookupResult.SingleValueResult(value)
              )
            }
            else {
              scope.findIdentifier(name, fileSpec, sourceLocation)
            }

        }

      if(param.elements.size > 1)
        param.elements.foldLeft(scope) { (scope, variable) => addParamElement(scope, variable) }
      else
        addVariable(param.paramVar)
    }

    def addParameters(params: Vector[Parameter]): Scope =
      params.foldLeft(scope) { (scope, param) => scope.addParameter(param) }

  }

  sealed trait NamespacesOnlyScope extends Scope {
    override def nextVariable: Int = 0
  }

  case object EmptyScope extends NamespacesOnlyScope {
    override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult] =
      context.compCompilationInstance.point(LookupResult.Failed)
  }

  final class NamespaceScope private(findId: ((String, FileSpec, SourceLocation)) => Comp[LookupResult], parentScope: NamespacesOnlyScope) extends NamespacesOnlyScope {

    override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Comp[LookupResult] =
      findId((name, fileSpec, sourceLocation))

  }

  object NamespaceScope {

    def apply(findId: (String, FileSpec, SourceLocation) => Comp[LookupResult], parentScope: NamespacesOnlyScope): Comp[NamespaceScope] =
      context.compCompilationInstance.map(
        context.compCompilationInstance.createMemo[(String, FileSpec, SourceLocation), LookupResult]
      ) { findIdMemo =>
        new NamespaceScope(findIdMemo(findId.tupled), parentScope)
      }

  }


  sealed trait ScopeValue {
    def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): Comp[other.ScopeValue]
  }
  sealed trait ScopeValueSingle extends ScopeValue {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): Comp[other.ScopeValueSingle]
  }
  sealed trait ScopeValueOverload extends ScopeValue {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): Comp[other.ScopeValueOverload]
  }
  final case class VariableScopeValue(variable: Variable) extends ScopeValueSingle {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): Comp[other.ScopeValueSingle] =
      converter.convertVariableTypeSystem(variable).map(other.VariableScopeValue)
  }
  final case class ParameterElementScopeValue(paramElem: ParameterElement) extends ScopeValueSingle {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): Comp[other.ScopeValueSingle] =
      converter.convertParameterElementTypeSystem(paramElem).map(other.ParameterElementScopeValue)
  }
  final case class FunctionScopeValue(func: AbsRef[context.type, ArFunc]) extends ScopeValueOverload {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): Comp[other.ScopeValueOverload] =
      other.FunctionScopeValue(func).pure[Comp]
  }
  final case class TraitScopeValue(arTrait: AbsRef[context.type, ArTrait]) extends ScopeValueOverload {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): Comp[other.ScopeValueOverload] =
      other.TraitScopeValue(arTrait).pure[Comp]
  }
  final case class ClassScopeValue(arClass: AbsRef[context.type, ArClass]) extends ScopeValueOverload {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): Comp[other.ScopeValueOverload] =
      other.ClassScopeValue(arClass).pure[Comp]
  }
  final case class DataConstructorScopeValue(ctor: AbsRef[context.type, DataConstructor]) extends ScopeValueOverload {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): Comp[other.ScopeValueOverload] =
      other.DataConstructorScopeValue(ctor).pure[Comp]
  }

  sealed trait LookupResult {
    def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): Comp[other.LookupResult]
  }
  object LookupResult {
    final case class ScopeResult(scope: Scope) extends LookupResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): Comp[other.LookupResult] =
        other.LookupResult.ScopeResult(scope.convertScopeContext(other)(converter)).pure[Comp]
    }
    final case class SingleValueResult(value: ScopeValueSingle) extends LookupResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): Comp[other.LookupResult] =
        value.convertScopeContext(other)(converter).map(other.LookupResult.SingleValueResult)
    }
    final case class ValuesResult(overloads: OverloadResult.List[ScopeValueOverload]) extends LookupResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): Comp[other.LookupResult] =
        overloads.traverse { _.convertScopeContext(other)(converter) }.map(other.LookupResult.ValuesResult)
    }
    case object Failed extends LookupResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter.Aux[context.type, typeSystem.type, other.typeSystem.type, Comp]): Comp[other.LookupResult] =
        other.LookupResult.Failed.pure[Comp]
    }
  }

}

