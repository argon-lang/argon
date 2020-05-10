package dev.argon.compiler.expr

import dev.argon.compiler.core.Context
import dev.argon.compiler.expr.ArExpr.TraitType

import scala.collection.immutable.Vector

final case class BaseTypeInfoTrait[TContext <: Context with Singleton, Wrap[+_]]
(
  baseTraits: Vector[TraitType[TContext, Wrap]]
)
