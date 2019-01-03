package com.mi3software.argon.compiler.lookup

import com.mi3software.argon.compiler._
import com.mi3software.argon.util.{FileSpec, NamespacePath, SourceLocation}
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.types.{TypeSystem, TypeSystemConverter}
import scalaz.Memo

trait ScopeContext[TContext <: Context with Singleton] {

  val context: TContext
  val typeSystem: TypeSystem[context.type]

  import typeSystem.Variable

  sealed trait Scope {

    def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): LookupResult

    def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type]): other.Scope = new other.Scope {
      override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): other.LookupResult =
        Scope.this.findIdentifier(name, fileSpec, sourceLocation).convertScopeContext(other)(converter)
    }

  }

  implicit final class ScopeExtensions(val scope: Scope) {

    def addVariable(variable: Variable[VariableLikeDescriptor]): Scope =
      ???

    def addVariables(variables: Vector[Variable[VariableLikeDescriptor]]): Scope =
      variables.foldLeft(scope) { (scope, variable) => scope.addVariable(variable) }

  }

  sealed trait NamespacesOnlyScope extends Scope

  case object EmptyScope extends NamespacesOnlyScope {
    override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): LookupResult =
      LookupResult.Failed
  }

  final class NamespaceScope(findId: (String, FileSpec, SourceLocation) => LookupResult, parentScope: NamespacesOnlyScope) extends NamespacesOnlyScope {

    private val findIdMemo = Memo.immutableHashMapMemo(Function.tupled(findId))

    override def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): LookupResult =
      findIdMemo((name, fileSpec, sourceLocation))
  }


  sealed trait ScopeValue {
    def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type]): other.ScopeValue
  }
  final case class VariableScopeValue(variable: Variable[VariableLikeDescriptor]) extends ScopeValue {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type]): other.ScopeValue =
      other.VariableScopeValue(TypeSystem.convertVariableTypeSystem(context)(typeSystem)(other.typeSystem)(converter)(variable))
  }
  final case class FunctionScopeValue(func: AbsRef[context.type, ArFunc]) extends ScopeValue {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type]): other.ScopeValue =
      other.FunctionScopeValue(func)
  }
  final case class TraitScopeValue(arTrait: AbsRef[context.type, ArTrait]) extends ScopeValue {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type]): other.ScopeValue =
      other.TraitScopeValue(arTrait)
  }
  final case class ClassScopeValue(arClass: AbsRef[context.type, ArClass]) extends ScopeValue {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type]): other.ScopeValue =
      other.ClassScopeValue(arClass)
  }
  final case class DataConstructorScopeValue(ctor: AbsRef[context.type, DataConstructor]) extends ScopeValue {
    override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type]): other.ScopeValue =
      other.DataConstructorScopeValue(ctor)
  }


  sealed trait LookupResult {
    def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type]): other.LookupResult
  }
  object LookupResult {
    final case class ScopeResult(scope: Scope) extends LookupResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type]): other.LookupResult =
        other.LookupResult.ScopeResult(scope.convertScopeContext(other)(converter))
    }
    final case class ValuesResult(overloads: OverloadResult) extends LookupResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type]): other.LookupResult =
        other.LookupResult.ValuesResult(overloads.convertScopeContext(other)(converter))
    }
    case object Failed extends LookupResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type]): other.LookupResult =
        other.LookupResult.Failed
    }
  }

  sealed trait OverloadResult {
    def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type]): other.OverloadResult
  }
  object OverloadResult {
    case object End extends OverloadResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type]): other.OverloadResult =
        other.OverloadResult.End
    }
    final case class List(values: Vector[ScopeValue], next: OverloadResult) extends OverloadResult {
      override def convertScopeContext(other: ScopeContext[context.type])(converter: TypeSystemConverter[context.type, typeSystem.type, other.typeSystem.type]): other.OverloadResult =
        other.OverloadResult.List(values.map { _.convertScopeContext(other)(converter) }, next.convertScopeContext(other)(converter))
    }
  }

}

