package dev.argon.compiler.expr

import dev.argon.compiler.core.Context
import dev.argon.compiler.expr.ArExpr.ClassConstructorCall
import dev.argon.util.Id

import scala.collection.immutable.Vector


sealed trait ClassConstructorStatement[TContext]
final case class ClassConstructorStatementExpr[TContext](expr: ArExpr[TContext, Id]) extends ClassConstructorStatement[TContext]

final case class InitializeFieldStatement[TContext <: Context with Singleton]
(
  field: FieldVariable[TContext, Id],
  value: ArExpr[TContext, Id]
) extends ClassConstructorStatement[TContext]

final case class ClassConstructorBody[TContext <: Context with Singleton]
(
  initStatements: Vector[ClassConstructorStatement[TContext]],
  baseConstructorCall: Option[ClassConstructorCall[TContext, Id]],
  endExpr: ArExpr[TContext, Id],
)
