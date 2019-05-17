package dev.argon.compiler

import scalaz.NonEmptyList

trait CompilationExec[F[_], G[_]] extends Compilation[F] {
  def getResult[A](fa: F[A]): G[(Vector[CompilationMessageNonFatal], Either[NonEmptyList[CompilationError], A])]
}
