package com.mi3software.argon.compiler

import scalaz.Leibniz

sealed trait ArMethod[TContext <: Context] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]

  val declaration: MethodDeclarationInfo[TContext]

  val effectInfo: EffectInfo

  val isVirtual: Boolean
  val isAbstract: Boolean
  val isImplicitOverride: Boolean
  val isFinal: Boolean

  def instanceTypeTemplate: ArTypeTemplate[TContext]

}

trait ArMethodDeclaration[TContext <: Context] extends ArMethod[TContext] {
  import context._

  override val declaration: MethodDeclarationInfoDeclaration[TContext]
  final override def instanceTypeTemplate: ArTypeTemplateDeclaration[TContext] = declaration.instanceType

  val implementation: Comp[context.TMethodImplementation]
}

trait ArMethodReference[TContext <: Context] extends ArMethod[TContext] {
  import context._

  override val declaration: MethodDeclarationInfoReference[TContext]
  final override def instanceTypeTemplate: ArTypeTemplateReference[TContext] = declaration.instanceType

  val contextMetadata: TMethodMetadata
}
