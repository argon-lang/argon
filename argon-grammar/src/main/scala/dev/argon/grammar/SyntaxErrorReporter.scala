package dev.argon.grammar

import dev.argon.util.NonEmptyVector

trait SyntaxErrorReporter[F[_], TSyntaxError] {
  def reportError[A](error: NonEmptyVector[TSyntaxError]): F[A]
}
