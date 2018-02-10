package com.mi3software.argon.compiler

import scala.collection.immutable._
import scalaz._
import Scalaz._

sealed trait ArClass[TContext <: Context] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]

  import context._

  val descriptor: ClassDescriptor

  val isOpen: Boolean
  val isSealed: Boolean
  val isAbstract: Boolean

  val methods: Comp[Vector[ArMethod[TContext]]]
  val classConstructors: Comp[Vector[ClassConstructor[TContext]]]
  val metaType: MetaClass[TContext, ArClass[TContext]]
}

object ArClass {

  implicit def equalInstance[TContext <: Context]: Equal[ArClass[TContext]] =
    (a, b) => a.descriptor === b.descriptor

}

trait ArClassDeclaration[TContext <: Context] extends ArClass[TContext] {
  import context._

  val metaType: MetaClass[TContext, ArClassDeclaration[TContext]]

  val methods: Comp[Vector[ArMethodDeclaration[TContext]]]
  val classConstructors: Comp[Vector[ClassConstructorDeclaration[TContext]]]
}

trait ArClassReference[TContext <: Context] extends ArClass[TContext] {
  import context._

  val metaType: MetaClass[TContext, ArClassReference[TContext]]

  val methods: Comp[Vector[ArMethodReference[TContext]]]
  val classConstructors: Comp[Vector[ClassConstructorReference[TContext]]]
  val contextMetadata: TClassMetadata
}

trait ArClassInNamespace[TContext <: Context] {
  self: ArClass[TContext] =>

  override val descriptor: ClassDescriptor.InNamespace
}
