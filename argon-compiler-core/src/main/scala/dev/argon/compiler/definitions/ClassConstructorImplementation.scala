package dev.argon.compiler.definitions

import dev.argon.compiler.*

sealed trait ClassConstructorImplementationC extends UsingContext
object ClassConstructorImplementationC {
  trait External extends ClassConstructorImplementationC {
    val name: String
    val impl: context.ExternClassConstructorImplementation
  }
  trait ExpressionBody extends ClassConstructorImplementationC {
    val preInitialization: Seq[Either[context.ExprContext.WrapExpr, FieldInitializationStatement & HasContext[context.type]]]
    val baseConstructorCall: BaseClassConstructorCallStatement & HasContext[context.type]
    val postInitialization: context.ExprContext.WrapExpr
  }

  trait FieldInitializationStatement extends UsingContext {
    val field: context.ExprContext.MemberVariable
    val value: context.ExprContext.WrapExpr
  }

  trait BaseClassConstructorCallStatement extends UsingContext {
    val baseCall: Option[context.ExprContext.ArExpr[context.ExprContext.ExprConstructor.ClassConstructorCall]]
    val instanceVariable: context.ExprContext.LocalVariable
  }
}
