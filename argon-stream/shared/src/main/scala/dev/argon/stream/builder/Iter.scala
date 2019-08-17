package dev.argon.stream.builder

import cats._
import cats.implicits._

trait Iter[F[_], -L[_, _], X0] {
  def foldLeftM[G[_]: Monad, A, X <: X0, S](convert: F ~> G)(data: L[A, X])(state: S)(f: (S, A) => G[S]): G[(S, X)]
  def foreach[G[_]: Monad, A, X <: X0](convert: F ~> G)(data: L[A, X])(f: A => G[Unit]): G[X] =
    foldLeftM(convert)(data)(()) { (_, a) => f(a) }.map { case (_, x) => x }

  def translateEffect[G[_]](convert: F ~> G): Iter[G, L, X0] = new Iter[G, L, X0] {
    override def foldLeftM[H[_] : Monad, A, X <: X0, S](convert2: G ~> H)(data: L[A, X])(state: S)(f: (S, A) => H[S]): H[(S, X)] =
      Iter.this.foldLeftM(convert.andThen(convert2))(data)(state)(f)
  }
}

object Iter {

  def apply[F[_], L[_, _], X0](implicit iter: Iter[F, L, X0]): Iter[F, L, X0] = iter

}