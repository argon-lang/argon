package dev.argon.util

trait Traverse[C[+_]] extends Functor[C] {
  def traverse[F[+_]: Applicative, A, B](ca: C[A])(f: A => F[B]): F[C[B]]
  def foldLeftM[F[+_]: Monad, S, A](ca: C[A])(s: S)(f: (S, A) => F[S]): F[S]
  def foldLeft[S, A](ca: C[A])(s: S)(f: (S, A) => S): S

  override def map[A, B](fa: C[A])(f: A => B): C[B] = traverse[Id, A, B](fa)(f)
}

extension [C[+_], A](ca: C[A])(using traverse: Traverse[C])
  def traverse[F[+_]: Applicative, B](f: A => F[B]): F[C[B]] = traverse.traverse(ca)(f)

  def foldLeftM[F[+_]: Monad, S, B](s: S)(f: (S, A) => F[S]): F[S] = traverse.foldLeftM(ca)(s)(f)
end extension

object Traverse:
  def apply[C[+_]](using instance: Traverse[C]): Traverse[C] = instance
end Traverse
