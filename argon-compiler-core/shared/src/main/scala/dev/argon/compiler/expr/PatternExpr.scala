package dev.argon.compiler.expr

import dev.argon.compiler.core.{AbsRef, Context, DataConstructor}

import scala.collection.immutable.Vector


sealed trait PatternExpr[TContext, Wrap[+_]]

object PatternExpr {
  final case class DataDeconstructor[TContext <: Context with Singleton, Wrap[+_]]
  (
    ctor: AbsRef[TContext, DataConstructor],
    args: Vector[PatternExpr[TContext, Wrap]]
  ) extends PatternExpr[TContext, Wrap]

  final case class Binding[TContext, Wrap[+_]](variable: LocalVariable[TContext, Wrap]) extends PatternExpr[TContext, Wrap]
  final case class CastBinding[TContext, Wrap[+_]](variable: LocalVariable[TContext, Wrap]) extends PatternExpr[TContext, Wrap]
}

final case class PatternCase[TContext, Wrap[+_]](pattern: PatternExpr[TContext, Wrap], body: ArExprWrap[TContext, Wrap])