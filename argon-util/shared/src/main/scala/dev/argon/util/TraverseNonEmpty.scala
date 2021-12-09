package dev.argon.util

trait TraverseNonEmpty[C[+_]] extends Traverse[C] {
  def reduceLeftM[F[+_]: Monad, A](ca: C[A])(f: (A, A) => F[A]): F[A]
  def reduceLeft[A](ca: C[A])(f: (A, A) => A): A
}

extension [C[+_], A](ca: C[A])(using traverse: TraverseNonEmpty[C])
  def reduceLeftM[F[+_]: Monad, S, B](f: (A, A) => F[A]): F[A] = traverse.reduceLeftM(ca)(f)

  def maximum(using order: Ordering[A]): A =
    traverse.reduceLeft(ca) { (x, y) =>
      if order.compare(x, y) < 0 then
        x
      else
        y
    }

end extension
