package com.mi3software.argon.compiler

import com.mi3software.argon.util.{FileSpec, NamespacePath, SourceLocation}

trait ScopeTypes {
  type TTrait
  type TClass
  type TDataConstructor
  type TFunc
  type TVariable
}

trait ScopeTypeConverter[-T1 <: ScopeTypes, +T2 <: ScopeTypes] {
  def convertTrait(t: T1#TTrait): T2#TTrait
  def convertClass(t: T1#TClass): T2#TClass
  def convertDataConstructor(t: T1#TDataConstructor): T2#TDataConstructor
  def convertFunc(t: T1#TFunc): T2#TFunc
  def convertVariable(t: T1#TVariable): T2#TVariable
}

sealed trait Scope[+Types <: ScopeTypes] {

  def findIdentifier(name: String, fileSpec: FileSpec, sourceLocation: SourceLocation): Lookup[ScopeValue[Types]] = ???

}

object ScopeHelpers {
  implicit final class ScopeExtensions[Types <: ScopeTypes](val scope: Scope[Types]) extends AnyVal {

    def addVariable(variable: Types#TVariable): Scope[Types] =
      ???

    def addVariables(variables: Vector[Types#TVariable]): Scope[Types] =
      variables.foldLeft(scope) { (scope, variable) => scope.addVariable(variable) }

    def convertTypes[T2 <: ScopeTypes](scopeTypeConverter: ScopeTypeConverter[Types, T2]): Scope[T2] = ???

  }
}

sealed trait NamespacesOnlyScope[+Types <: ScopeTypes] extends Scope[Types] {

  final def addNamespaces(namespacePaths: Vector[NamespacePath]): NamespaceScope[Types] =
    NamespaceScope(namespacePaths, this)

}

final case class EmptyScope[+Types <: ScopeTypes]() extends NamespacesOnlyScope[Types]

final case class NamespaceScope[+Types <: ScopeTypes](namespacePaths: Vector[NamespacePath], parentScope: NamespacesOnlyScope[Types]) extends NamespacesOnlyScope[Types]


sealed trait ScopeValue[+Types <: ScopeTypes]

final case class NamespaceScopeValue[+Types <: ScopeTypes](ns: Namespace[ScopeValue[Types]]) extends ScopeValue[Types]


sealed trait NonNamespaceScopeValue[+Types <: ScopeTypes] extends ScopeValue[Types]


final case class VariableScopeValue[+Types <: ScopeTypes](variable: Types#TVariable) extends NonNamespaceScopeValue[Types]
final case class FunctionScopeValue[+Types <: ScopeTypes](func: Types#TFunc) extends NonNamespaceScopeValue[Types]
final case class TraitScopeValue[+Types <: ScopeTypes](arTrait: Types#TTrait) extends NonNamespaceScopeValue[Types]
final case class ClassScopeValue[+Types <: ScopeTypes](arClass: Types#TClass) extends NonNamespaceScopeValue[Types]
final case class DataConstructorScopeValue[+Types <: ScopeTypes](ctor: Types#TDataConstructor) extends NonNamespaceScopeValue[Types]

