package com.mi3software.argon.compiler

import scala.collection.immutable._
import scalaz.Leibniz

sealed trait ArTrait[TContext <: Context] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._

  val declaration: TraitDeclarationInfo[TContext]

  val isSealed: Boolean

  val methods: Comp[Vector[ArMethod[TContext]]]
  val metaType: MetaClass[TContext, ArClass[TContext]]
}

trait ArTraitDeclaration[TContext <: Context] extends ArTrait[TContext] {
  import context._

  val metaType: MetaClass[TContext, ArClassDeclaration[TContext]]

  val methods: Comp[Vector[ArMethodDeclaration[TContext]]]
}

trait ArTraitReference[TContext <: Context] extends ArTrait[TContext] {
  import context._

  val metaType: MetaClass[TContext, ArClassReference[TContext]]

  val methods: Comp[Vector[ArMethodReference[TContext]]]
  val contextMetadata: context.TTraitMetadata
}

trait ArTraitInNamespace[TContext <: Context] {
  self: ArTrait[TContext] =>

  override val declaration: TraitDeclarationInNamespace[TContext]
}

