package dev.argon.util

trait Applicative[F[+_]] extends Functor[F]:
  def ap[A, B](fa: F[A])(fab: F[A => B]): F[B]
  def pure[A](a: A): F[A]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    ap[A, Z](fa) {
      map(fb) { b =>
        a => f(a, b)
      }
    }

end Applicative

object Applicative:
  def apply[F[+_]](using ap: Applicative[F]): Applicative[F] = ap
end Applicative



