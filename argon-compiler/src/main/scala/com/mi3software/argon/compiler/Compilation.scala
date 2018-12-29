package com.mi3software.argon.compiler

import scalaz._
import Scalaz._

trait Compilation[F[_]] extends Monad[F] {

  def diagnostic[A](value: A, messages: Vector[CompilationMessageNonFatal]): F[A]
  def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[CompilationMessageNonFatal] = Vector()): F[A]
  final def forErrors[A](head: CompilationError, tail: CompilationError*): F[A] =
    forErrors(NonEmptyList.nel(head, IList(tail: _*)))

  override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]

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

