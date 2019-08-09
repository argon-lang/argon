package dev.argon.compiler

import cats._
import cats.implicits._
import cats.data.NonEmptyList

trait Compilation[F[_]] extends Monad[F] {

  def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[Nothing] = Vector()): F[A]
  final def forErrors[A](head: CompilationError, tail: CompilationError*): F[A] =
    forErrors(NonEmptyList(head, tail.toList))

  def createCache[A]: F[F[A] => F[A]]
  def createMemo[A, B]: F[(A => F[B]) => A => F[B]]

  def attempt[A](action: F[A]): F[Either[NonEmptyList[CompilationError], F[A]]]

  final def require(value: Boolean)(head: CompilationError, tail: CompilationError*): F[Unit] =
    if(value) point(())
    else forErrors(head, tail: _*)

  final def requireSome[A](option: Option[A])(head: CompilationError, tail: CompilationError*): F[A] =
    option match {
      case Some(a) => point(a)
      case None => forErrors(head, tail: _*)
    }

}



object Compilation {

  def apply[F[_] : Compilation]: Compilation[F] = implicitly[Compilation[F]]

  object Operators {

    implicit class BoolOperators[F[_]](private val left: F[Boolean]) extends AnyVal {

      def && (right: => F[Boolean])(implicit comp: Compilation[F]): F[Boolean] =
        left.flatMap {
          case true => right
          case false => false.pure[F]
        }

      def || (right: => F[Boolean])(implicit comp: Compilation[F]): F[Boolean] =
        left.flatMap {
          case true => true.pure[F]
          case false => right
        }

    }

  }

}

