package dev.argon.stream.builder

import cats._
import cats.implicits._

trait Iter[F[_], -L[_, _], X0] {
  def foldLeftM[A, X <: X0, S](data: L[A, X])(state: S)(f: (S, A) => F[S]): F[(S, X)]
  def foreach[A, X <: X0](data: L[A, X])(f: A => F[Unit])(implicit functor: Functor[F]): F[X] =
    foldLeftM(data)(()) { (_, a) => f(a) }.map { case (_, x) => x }

  def foldLeftMHandlerFunc[A, X <: X0](data: L[A, X]): GenEffect.HandlerFunctionState[F, A, X] =
    new GenEffect.HandlerFunctionState[F, A, X] {
      override def apply[S](state: S)(f: (S, A) => F[S]): F[(S, X)] =
        foldLeftM(data)(state)(f)
    }

  def foreachG[G[_], A, X <: X0](data: L[A, X])(f: A => G[Unit])(implicit functor: Functor[G], genEffect: GenEffect[F, G]): G[X] =
    genEffect.liftFuncState(foldLeftMHandlerFunc(data))(()) { (_, a) => f(a) }.map { case (_, x) => x }
}

object Iter {

  def apply[F[_], L[_, _], X0](implicit iter: Iter[F, L, X0]): Iter[F, L, X0] = iter

}