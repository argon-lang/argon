package com.mi3software.argon.compiler

import scala.collection.immutable._
import scalaz._
import Scalaz._


sealed trait DataConstructor[TContext <: Context] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._

  val descriptor: DataConstructorDescriptor

  val methods: Comp[Vector[ArMethod[TContext]]]
}

object DataConstructor {

  implicit def equalInstance[TContext <: Context]: Equal[DataConstructor[TContext]] =
    (a, b) => a.descriptor === b.descriptor

}

trait DataConstructorDeclaration[TContext <: Context] extends DataConstructor[TContext] {
  import context._

  val methods: Comp[Vector[ArMethodDeclaration[TContext]]]
  val implementation: Comp[TConstructorImplementation]
}

trait DataConstructorReference[TContext <: Context] extends DataConstructor[TContext] {
  import context._

  val methods: Comp[Vector[ArMethodReference[TContext]]]
  val contextMetadata: TConstructorMetadata
}
