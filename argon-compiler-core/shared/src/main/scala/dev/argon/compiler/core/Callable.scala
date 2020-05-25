package dev.argon.compiler.core

sealed trait Callable
sealed trait NonExpressionCallable extends Callable {
  val id: CallableId
}
trait CallableClass extends NonExpressionCallable
trait CallableTrait extends NonExpressionCallable
trait CallableDataConstructor extends NonExpressionCallable
trait CallableFunction extends NonExpressionCallable
trait CallableMethod extends NonExpressionCallable
trait CallableClassConstructor extends NonExpressionCallable
case object CallableExpression extends Callable

