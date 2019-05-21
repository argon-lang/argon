package dev.argon.compiler

import scalaz.NonEmptyList

trait CompilationExec[F[_, _], G[_]] extends CompilationE[F] {
  def getResult[A](fa: F[NonEmptyList[CompilationError], A]): G[(Vector[CompilationMessageNonFatal], Either[NonEmptyList[CompilationError], A])]
}
