package dev.argon.compiler.options

import cats.Applicative

trait OptionsConverter[Options[_[_]]] {
  def convert[A[_], B[_], C[_], F[_]: Applicative](optionsA: Options[A], optionsB: Options[B])(f: OptionsConverterFunction[A, B, C, F]): F[Options[C]]
}

trait OptionsConverterFunction[A[_], B[_], C[_], F[_]] {
  def apply[X](ax: A[X], bx: B[X]): F[C[X]]
}
