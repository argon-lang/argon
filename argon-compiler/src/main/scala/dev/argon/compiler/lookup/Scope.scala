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

  import typeSystem.Variable

  sealed trait Scope {

    def nextVariable: Int

    def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): context.Comp[LookupResult]

    def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type, Id]): other.Scope = new other.Scope {

      override def nextVariable: Int = Scope.this.nextVariable

      override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): context.Comp[other.LookupResult] =
        context.compCompilationInstance.map(
          Scope.this.findIdentifier(name, fileSpec, sourceLocation)
        ) { _.convertScopeContext(other)(converter) }
    }

  }

  implicit final class ScopeExtensions(val scope: Scope) {

    def addVariable(variable: Variable): Scope =
      new Scope {
        override def nextVariable: Int = variable.descriptor match {
          case VariableDescriptor(_, id) => id + 1
          case _ => scope.nextVariable
        }

        override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): context.Comp[LookupResult] =
          if(variable.name === VariableName.Normal(name)) {
            val value = VariableScopeValue(variable)
            context.compCompilationInstance.point(
              LookupResult.ValuesResult(OverloadResult.List(Vector(value), OverloadResult.End))
            )
          }
          else {
            scope.findIdentifier(name, fileSpec, sourceLocation)
          }

      }

    def addVariables(variables: Vector[Variable]): Scope =
      variables.foldLeft(scope) { (scope, variable) => scope.addVariable(variable) }

  }

  sealed trait NamespacesOnlyScope extends Scope {
    override def nextVariable: Int = 0
  }

  case object EmptyScope extends NamespacesOnlyScope {
    override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): context.Comp[LookupResult] =
      context.compCompilationInstance.point(LookupResult.Failed)
  }

  final class NamespaceScope private(findId: ((String, FileSpec, SourceLocation)) => context.Comp[LookupResult], parentScope: NamespacesOnlyScope) extends NamespacesOnlyScope {

    override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): context.Comp[LookupResult] =
      findId((name, fileSpec, sourceLocation))

  }

  object NamespaceScope {

    def apply(findId: (String, FileSpec, SourceLocation) => context.Comp[LookupResult], parentScope: NamespacesOnlyScope): context.Comp[NamespaceScope] =
      context.compCompilationInstance.map(
        context.compCompilationInstance.createMemo[(String, FileSpec, SourceLocation), LookupResult]
      ) { findIdMemo =>
        new NamespaceScope(findIdMemo(findId.tupled), parentScope)
      }

  }


  sealed trait ScopeValue {
    def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type, Id]): other.ScopeValue
  }
  final case class VariableScopeValue(variable: Variable) extends ScopeValue {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type, Id]): other.ScopeValue =
      other.VariableScopeValue(TypeSystem.convertVariableTypeSystem(context)(typeSystem)(other.typeSystem)(converter)(variable))
  }
  final case class FunctionScopeValue(func: AbsRef[context.type, ArFunc]) extends ScopeValue {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type, Id]): other.ScopeValue =
      other.FunctionScopeValue(func)
  }
  final case class TraitScopeValue(arTrait: AbsRef[context.type, ArTrait]) extends ScopeValue {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type, Id]): other.ScopeValue =
      other.TraitScopeValue(arTrait)
  }
  final case class ClassScopeValue(arClass: AbsRef[context.type, ArClass]) extends ScopeValue {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type, Id]): other.ScopeValue =
      other.ClassScopeValue(arClass)
  }
  final case class DataConstructorScopeValue(ctor: AbsRef[context.type, DataConstructor]) extends ScopeValue {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type, Id]): other.ScopeValue =
      other.DataConstructorScopeValue(ctor)
  }

  sealed trait LookupResult {
    def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type, Id]): other.LookupResult
  }
  object LookupResult {
    final case class ScopeResult(scope: Scope) extends LookupResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type, Id]): other.LookupResult =
        other.LookupResult.ScopeResult(scope.convertScopeContext(other)(converter))
    }
    final case class ValuesResult(overloads: OverloadResult[ScopeValue]) extends LookupResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type, Id]): other.LookupResult =
        other.LookupResult.ValuesResult(overloads.map { _.convertScopeContext(other)(converter) })
    }
    case object Failed extends LookupResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type, Id]): other.LookupResult =
        other.LookupResult.Failed
    }
  }

}

