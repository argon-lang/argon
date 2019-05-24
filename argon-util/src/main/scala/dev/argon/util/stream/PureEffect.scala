package dev.argon.util.stream

import cats._

final case class PureEffect[-R, +E, +A](run: R => Eval[Either[E, A]])

object PureEffect {

  def apply[A](value: A): PureEffect[Any, Nothing, A] = PureEffect(_ => Eval.now(Right(value)))
  def fail[E](error: E): PureEffect[Any, E, Nothing] = PureEffect(_ => Eval.now(Left(error)))
  def fromEither[E, A](either: Either[E, A]): PureEffect[Any, E, A] = either match {
    case Left(error) => fail(error)
    case Right(value) => apply(value)
  }

  implicit def pureEffectMonadInstance[R, E]: Monad[PureEffect[R, E, ?]] = new StackSafeMonad[PureEffect[R, E, ?]] {
    override def flatMap[A, B](fa: PureEffect[R, E, A])(f: A => PureEffect[R, E, B]): PureEffect[R, E, B] =
      PureEffect(r => fa.run(r).flatMap {
        case Right(a) => f(a).run(r)
        case Left(e) => Eval.now(Left(e))
      })

    override def pure[A](x: A): PureEffect[R, E, A] =
      PureEffect(x)
  }

}
