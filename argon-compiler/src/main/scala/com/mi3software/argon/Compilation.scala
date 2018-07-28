package com.mi3software.argon

import com.mi3software.argon.compiler.{CompilationError, CompilationMessageNonFatal}
import scalaz.{IList, Monad, NonEmptyList}

trait Compilation[F[_]] extends Monad[F] {

  def diagnostic[A](value: A, messages: Vector[CompilationMessageNonFatal]): F[A]
  def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[CompilationMessageNonFatal] = Vector()): F[A]
  final def forErrors[A](head: CompilationError, tail: CompilationError*): F[A] =
    forErrors(NonEmptyList.nel(head, IList(tail: _*)))

  override def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B]

}



object Compilation {

  def apply[F[_] : Compilation]: Compilation[F] = implicitly[Compilation[F]]

}

