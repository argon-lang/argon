package dev.argon.stream.builder

import dev.argon.stream.builder.GenEffect.{HandlerFunction, HandlerFunctionState}

trait GenEffect[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
  def liftFunc[A](f: HandlerFunction[F, A]): HandlerFunction[G, A]
  def liftFuncState[A, X](f: HandlerFunctionState[F, A, X]): HandlerFunctionState[G, A, X]
}

object GenEffect {

  trait HandlerFunction[F[_], A] {
    def apply[B](f: A => F[B]): F[B]
  }

  trait HandlerFunctionState[F[_], A, X] {
    def apply[S](state: S)(f: (S, A) => F[S]): F[(S, X)]
  }

  implicit def genEffectComposed[F[_], G[_], H[_]](implicit gen1: GenEffect[F, G], gen2: GenEffect[G, H]): GenEffect[F, H] =
    new GenEffect[F, H] {
      override def apply[A](fa: F[A]): H[A] = gen2(gen1(fa))

      override def liftFunc[A](f: HandlerFunction[F, A]): HandlerFunction[H, A] =
        gen2.liftFunc(gen1.liftFunc(f))

      override def liftFuncState[A, X](f: HandlerFunctionState[F, A, X]): HandlerFunctionState[H, A, X] =
        gen2.liftFuncState(gen1.liftFuncState(f))
    }

}
