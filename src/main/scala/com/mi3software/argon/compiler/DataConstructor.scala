package com.mi3software.argon.compiler

import scala.collection.immutable._
import scalaz.Leibniz


sealed trait DataConstructor[TContext <: Context] {
  val context: TContext
  val contextProof: Leibniz[context.type, TContext, context.type, TContext]
  import context._

  val declaration: ConstructorDeclarationInfo[TContext]

  val methods: Comp[Vector[ArMethod[TContext]]]
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
