package dev.argon.stream.builder

trait Iter[F[_], L[_]] {
  def foldLeftM[A, S](data: L[A])(state: S)(f: (S, A) => F[S]): F[S]
}
