package dev.argon.compiler.expr

import dev.argon.compiler.core.Context
import dev.argon.compiler.expr.ArExpr.{ClassType, TraitType}

import scala.collection.immutable.Vector

final case class BaseTypeInfoClass[TContext <: Context with Singleton, Wrap[+_]]
(
  baseClass: Option[ClassType[TContext, Wrap]],
  baseTraits: Vector[TraitType[TContext, Wrap]]
)
