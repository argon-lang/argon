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

    def convertScopeContext(other: ScopeContext[context.type])(f: typeSystem.TType => other.typeSystem.TType): other.Scope = ???

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


  sealed trait ScopeValue
  final case class VariableScopeValue(variable: Variable[VariableLikeDescriptor]) extends ScopeValue
  final case class FunctionScopeValue(func: AbsRef[context.type, ArFunc]) extends ScopeValue
  final case class TraitScopeValue(arTrait: AbsRef[context.type, ArTrait]) extends ScopeValue
  final case class ClassScopeValue(arClass: AbsRef[context.type, ArClass]) extends ScopeValue
  final case class DataConstructorScopeValue(ctor: AbsRef[context.type, DataConstructor]) extends ScopeValue


  sealed trait LookupResult
  object LookupResult {
    final case class ScopeResult(scope: Scope) extends LookupResult
    final case class ValuesResult(overloads: OverloadResult) extends LookupResult
    case object Failed extends LookupResult
  }

  sealed trait OverloadResult
  object OverloadResult {
    case object End extends OverloadResult
    final case class List(values: Vector[ScopeValue], next: OverloadResult) extends OverloadResult
  }

}

