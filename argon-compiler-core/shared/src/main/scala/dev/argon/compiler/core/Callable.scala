package dev.argon.compiler.core

sealed trait Callable
sealed trait NonExpressionCallable extends Callable {
  val id: CallableId
}
@SuppressWarnings(Array("scalafix:MissingFinal.trait"))
trait CallableClass extends NonExpressionCallable
@SuppressWarnings(Array("scalafix:MissingFinal.trait"))
trait CallableTrait extends NonExpressionCallable
@SuppressWarnings(Array("scalafix:MissingFinal.trait"))
trait CallableDataConstructor extends NonExpressionCallable
@SuppressWarnings(Array("scalafix:MissingFinal.trait"))
trait CallableFunction extends NonExpressionCallable
@SuppressWarnings(Array("scalafix:MissingFinal.trait"))
trait CallableMethod extends NonExpressionCallable
@SuppressWarnings(Array("scalafix:MissingFinal.trait"))
trait CallableClassConstructor extends NonExpressionCallable
case object CallableExpression extends Callable

