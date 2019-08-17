package dev.argon.grammar

trait ParseErrorHandler[F[_], E] {
  def raiseError[A](error: E): F[A]
}
