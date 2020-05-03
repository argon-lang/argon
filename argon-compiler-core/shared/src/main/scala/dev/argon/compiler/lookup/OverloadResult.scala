package dev.argon.compiler.lookup

import cats._
import cats.data.{NonEmptyList, NonEmptyVector}
import cats.implicits._


sealed trait OverloadResult[+T] {
  def map[U](f: T => U): OverloadResult[U]

  def toList: List[NonEmptyVector[T]]

  def traverse[F[+_]: Monad, U](f: T => F[U]): F[OverloadResult[U]]
}
object OverloadResult {
  case object End extends OverloadResult[Nothing] {
    override def map[U](f: Nothing => U): OverloadResult[U] = End
    override def toList: scala.List[NonEmptyVector[Nothing]] = Nil

    override def traverse[F[+_] : Monad, U](f: Nothing => F[U]): F[End.type] =
      End.pure[F]
  }
  final case class List[+T](values: NonEmptyVector[T], next: OverloadResult[T]) extends OverloadResult[T] {

    override def map[U](f: T => U): OverloadResult.List[U] =
      List(values.map(f), next.map(f))

    override def toList: scala.List[NonEmptyVector[T]] =
      values :: next.toList

    def toNonEmptyList: NonEmptyList[NonEmptyVector[T]] =
      NonEmptyList(values, next.toList)

    def traverse[F[+_]: Monad, U](f: T => F[U]): F[OverloadResult.List[U]] =
      for {
        newValues <- values.traverse(f)
        newNext <- next.traverse(f)
      } yield List(newValues, newNext)

  }
}
