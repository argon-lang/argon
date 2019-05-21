package dev.argon.compiler

import scalaz.NonEmptyList

trait CompilationE[F[_, _]] extends Compilation[F[NonEmptyList[CompilationError], ?]] {
  type ErrorType = NonEmptyList[CompilationError]
}
