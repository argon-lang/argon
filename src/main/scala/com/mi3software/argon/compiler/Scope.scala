package com.mi3software.argon.compiler

import com.mi3software.argon.util.NamespacePath

sealed trait Scope[TContext <: Context] {

}

sealed trait NamespacesOnlyScope[TContext <: Context] extends Scope[TContext] {

  def addNamespaces(namespacePaths: Vector[NamespacePath]): NamespaceScope[TContext]

}

sealed trait EmptyScope[TContext <: Context] extends NamespacesOnlyScope[TContext]

object EmptyScope {
  def apply(context: Context): EmptyScope[context.type] = new EmptyScope[context.type] {

    override def addNamespaces(namespacePaths: Vector[NamespacePath]): NamespaceScope[context.type] =
      NamespaceScope(context)(namespacePaths, this)
  }
}

sealed trait NamespaceScope[TContext <: Context] extends NamespacesOnlyScope[TContext]

object NamespaceScope {

  def apply(context: Context)(namespacePaths: Vector[NamespacePath], parentScope: Scope[context.type]): NamespaceScope[context.type] = new NamespaceScope[context.type] {

    override def addNamespaces(namespacePaths: Vector[NamespacePath]): NamespaceScope[context.type] =
      NamespaceScope(context)(namespacePaths, this)

  }

}


sealed trait ScopeValueTypes {
  type TTrait[TContext <: Context] <: ArTrait[TContext]
  type TClass[TContext <: Context] <: ArClass[TContext]
  type TConstructor[TContext <: Context] <: DataConstructor[TContext]
  type TFunc[TContext <: Context] <: ArFunc[TContext]
  type TVariable[TContext <: Context]
}

sealed trait ScopeValueTypesVariable extends ScopeValueTypes {
  override type TTrait[TContext <: Context] = Nothing
  override type TClass[TContext <: Context] = Nothing
  override type TConstructor[TContext <: Context] = Nothing
  override type TFunc[TContext <: Context] = Nothing
  override type TVariable[TContext <: Context] = Nothing
}

sealed trait ScopeValueTypesInNamespace extends ScopeValueTypes {
  override type TTrait[TContext <: Context] <: ArTrait[TContext] with ArTraitInNamespace[TContext]
  override type TClass[TContext <: Context] <: ArClass[TContext] with ArClassInNamespace[TContext]
  override type TConstructor[TContext <: Context] <: DataConstructor[TContext]
  override type TFunc[TContext <: Context] <: ArFunc[TContext] with ArFuncInNamespace[TContext]
  override type TVariable[TContext <: Context] = Nothing
}

sealed trait ScopeValueTypesDeclaration extends ScopeValueTypes {
  override type TTrait[TContext <: Context] <: ArTraitDeclaration[TContext]
  override type TClass[TContext <: Context] <: ArClassDeclaration[TContext]
  override type TConstructor[TContext <: Context] <: DataConstructorDeclaration[TContext]
  override type TFunc[TContext <: Context] <: ArFuncDeclaration[TContext]
}

sealed trait ScopeValueTypesDeclarationInNamespace extends ScopeValueTypesDeclaration with ScopeValueTypesInNamespace {
  override type TTrait[TContext <: Context] = ArTraitDeclaration[TContext] with ArTraitInNamespace[TContext]
  override type TClass[TContext <: Context] = ArClassDeclaration[TContext] with ArClassInNamespace[TContext]
  override type TConstructor[TContext <: Context] = DataConstructorDeclaration[TContext]
  override type TFunc[TContext <: Context] = ArFuncDeclaration[TContext] with ArFuncInNamespace[TContext]
  override type TVariable[TContext <: Context] = Nothing
}

sealed trait ScopeValueTypesReference extends ScopeValueTypes {
  override type TTrait[TContext <: Context] <: ArTraitReference[TContext]
  override type TClass[TContext <: Context] <: ArClassReference[TContext]
  override type TConstructor[TContext <: Context] <: DataConstructorReference[TContext]

  override type TFunc[TContext <: Context] <: ArFuncReference[TContext]
}

sealed trait ScopeValueTypesReferenceInNamespace extends ScopeValueTypesReference with ScopeValueTypesInNamespace {
  override type TTrait[TContext <: Context] = ArTraitReference[TContext] with ArTraitInNamespace[TContext]
  override type TClass[TContext <: Context] = ArClassReference[TContext] with ArClassInNamespace[TContext]
  override type TConstructor[TContext <: Context] = DataConstructorReference[TContext]
  override type TFunc[TContext <: Context] = ArFuncReference[TContext] with ArFuncInNamespace[TContext]
  override type TVariable[TContext <: Context] = Nothing
}



sealed trait ScopeValue[TContext <: Context, +TScopeValueTypes <: ScopeValueTypes]

sealed trait NonNamespaceScopeValue[TContext <: Context, +TScopeValueTypes <: ScopeValueTypes] extends ScopeValue[TContext, TScopeValueTypes]

final case class VariableScopeValue[TContext <: Context, +TScopeValueTypes <: ScopeValueTypes](variable: TScopeValueTypes#TVariable[TContext]) extends NonNamespaceScopeValue[TContext, TScopeValueTypes]

final case class NamespaceScopeValue[TContext <: Context, +TScopeValueTypes <: ScopeValueTypes](ns: Namespace[ScopeValue[TContext, TScopeValueTypes]]) extends ScopeValue[TContext, TScopeValueTypes]

final case class FunctionScopeValue[TContext <: Context, +TScopeValueTypes <: ScopeValueTypes](func: TScopeValueTypes#TFunc[TContext]) extends NonNamespaceScopeValue[TContext, TScopeValueTypes]

final case class TraitScopeValue[TContext <: Context, +TScopeValueTypes <: ScopeValueTypes](arTrait: TScopeValueTypes#TTrait[TContext]) extends NonNamespaceScopeValue[TContext, TScopeValueTypes]

final case class ClassScopeValue[TContext <: Context, +TScopeValueTypes <: ScopeValueTypes](arClass: TScopeValueTypes#TClass[TContext]) extends NonNamespaceScopeValue[TContext, TScopeValueTypes]

final case class ConstructorScopeValue[TContext <: Context, +TScopeValueTypes <: ScopeValueTypes](ctor: TScopeValueTypes#TConstructor[TContext]) extends NonNamespaceScopeValue[TContext, TScopeValueTypes]

