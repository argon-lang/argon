package com.mi3software.argon.compiler

import com.mi3software.argon.util.{FileSpec, NamespacePath, SourceLocation}

trait ScopeTypes {
  type TTrait
  type TClass
  type TDataConstructor
  type TFunc
  type TVariable
}

sealed trait Scope[+Types <: ScopeTypes] {

  def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Lookup[ScopeValue[Types]] = ???

}

object ScopeHelpers {
  implicit class ScopeExtensions[Types <: ScopeTypes](val scope: Scope[Types]) extends AnyVal {

    final def addVariable(variable: Types#TVariable): Scope[Types] =
      ???

  }
}

sealed trait NamespacesOnlyScope[+Types <: ScopeTypes] extends Scope[Types] {

  final def addNamespaces(namespacePaths: Vector[NamespacePath]): NamespaceScope[Types] =
    NamespaceScope(namespacePaths, this)

}

final case class EmptyScope[+Types <: ScopeTypes]() extends NamespacesOnlyScope[Types]

final case class NamespaceScope[+Types <: ScopeTypes](namespacePaths: Vector[NamespacePath], parentScope: NamespacesOnlyScope[Types]) extends NamespacesOnlyScope[Types]



sealed trait ScopeValueTypesInNamespace[+PrevTypes <: ScopeTypes] extends ScopeTypes {
  override type TTrait <: PrevTypes#TTrait
  override type TClass <: PrevTypes#TClass
  override type TDataConstructor <: PrevTypes#TDataConstructor
  override type TFunc <: PrevTypes#TFunc
  override type TVariable = Nothing
}

sealed trait ScopeValue[+Types <: ScopeTypes]

final case class NamespaceScopeValue[+Types <: ScopeTypes](ns: Namespace[ScopeValue[ScopeValueTypesInNamespace[Types]]]) extends ScopeValue[Types]


sealed trait NonNamespaceScopeValue[+Types <: ScopeTypes] extends ScopeValue[Types]


final case class VariableScopeValue[+Types <: ScopeTypes](variable: Types#TVariable) extends ScopeValue[Types]
final case class FunctionScopeValue[+Types <: ScopeTypes](func: Types#TFunc) extends ScopeValue[Types]
final case class TraitScopeValue[+Types <: ScopeTypes](arTrait: Types#TTrait) extends ScopeValue[Types]
final case class ClassScopeValue[+Types <: ScopeTypes](arClass: Types#TClass) extends ScopeValue[Types]
final case class DataConstructorScopeValue[+Types <: ScopeTypes](ctor: Types#TDataConstructor) extends ScopeValue[Types]

