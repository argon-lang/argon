package dev.argon.compiler

import cats.data.NonEmptyList

trait CompilationExec[F[_, _, _], R] extends CompilationRE[F, R] {
  def getResult[A](fa: F[R, NonEmptyList[CompilationError], A]): F[R, Nothing, (Vector[CompilationMessageNonFatal], Either[NonEmptyList[CompilationError], A])]
}
