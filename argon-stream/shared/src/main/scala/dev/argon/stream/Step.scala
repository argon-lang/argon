package dev.argon.stream

sealed trait StepPure[+S, +E, +A, +B, +R]
sealed trait Step[+S, +A, +B, +R] extends StepPure[S, Nothing, A, B, R]
object Step {
  final case class Produce[+S, +A, +B](state: S, value: B, chunk: Vector[A]) extends Step[S, A, B, Nothing]
  final case class Continue[+S](state: S) extends Step[S, Nothing, Nothing, Nothing]
  final case class Stop[+R](result: R) extends Step[Nothing, Nothing, Nothing, R]
  final case class Fail[+E](error: E) extends StepPure[Nothing, E, Nothing, Nothing, Nothing]
}