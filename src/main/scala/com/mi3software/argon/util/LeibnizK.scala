package com.mi3software.argon.util

sealed abstract class LeibnizK[F[_], G[_]] {
  def apply[T](f: F[T]): G[T]
  def subst[H[_[_]]](h: H[F]): H[G]
  def flip: LeibnizK[G, F]
}

object LeibnizK {

  implicit def refl[F[_]]: LeibnizK[F, F] = new LeibnizK[F, F] {
    override def apply[T](f: F[T]): F[T] = f
    override def subst[H[_[_]]](h: H[F]): H[F] = h
    override def flip: LeibnizK[F, F] = this
  }

}
