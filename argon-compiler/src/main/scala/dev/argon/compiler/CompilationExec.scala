package dev.argon.compiler

import cats.data.NonEmptyList

trait CompilationExec[F[_, _, _], G[_]] extends CompilationRE[F] {
  def getResult[A](fa: F[Any, NonEmptyList[CompilationError], A]): G[(Vector[CompilationMessageNonFatal], Either[NonEmptyList[CompilationError], A])]
}
