package com.mi3software.argon.compiler

import scala.collection.immutable._
import scalaz.Leibniz

sealed trait ArClass[TContext <: Context] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]

  import context._

  val declaration: ClassDeclarationInfo[TContext]

  val isOpen: Boolean
  val isSealed: Boolean
  val isAbstract: Boolean

  val methods: Comp[Vector[ArMethod[TContext]]]
  val classConstructors: Comp[Vector[ClassConstructor[TContext]]]
  val metaType: MetaClass[TContext, ArClass[TContext]]
}

trait ArClassDeclaration[TContext <: Context] extends ArClass[TContext] {
  import context._

  val declaration: ClassDeclarationInfoDeclaration[TContext]
  val metaType: MetaClass[TContext, ArClassDeclaration[TContext]]

  val methods: Comp[Vector[ArMethodDeclaration[TContext]]]
  val classConstructors: Comp[Vector[ClassConstructorDeclaration[TContext]]]
}

trait ArClassReference[TContext <: Context] extends ArClass[TContext] {
  import context._

  val declaration: ClassDeclarationInfoReference[TContext]
  val metaType: MetaClass[TContext, ArClassReference[TContext]]

  val methods: Comp[Vector[ArMethodReference[TContext]]]
  val classConstructors: Comp[Vector[ClassConstructorReference[TContext]]]
  val contextMetadata: TClassMetadata
}

trait ArClassInNamespace[TContext <: Context] {
  self: ArClass[TContext] =>

  val declaration: ClassDeclarationInNamespace[TContext]
}
