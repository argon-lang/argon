package com.mi3software.argon.grammar

import com.mi3software.argon.util.NonEmptyVector

trait SyntaxErrorReporter[F[_], TSyntaxError] {
  def reportError[A](error: NonEmptyVector[TSyntaxError]): F[A]
}
