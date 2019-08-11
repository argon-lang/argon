package dev.argon.compiler

import cats.data.NonEmptyList

trait CompilationE[F[_, _]] extends Compilation[F[NonEmptyList[CompilationError], ?]] {
  type ErrorType = NonEmptyList[CompilationError]
}

object CompilationE {

  def apply[F[_, _] : CompilationE]: CompilationE[F] = implicitly[CompilationE[F]]

}
