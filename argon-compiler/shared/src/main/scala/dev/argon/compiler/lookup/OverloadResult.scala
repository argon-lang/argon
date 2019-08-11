package dev.argon.compiler.lookup

import cats.data.{NonEmptyList, NonEmptyVector}


sealed trait OverloadResult[+T] {
  def map[U](f: T => U): OverloadResult[U]

  def toList: List[NonEmptyVector[T]]
}
object OverloadResult {
  case object End extends OverloadResult[Nothing] {
    override def map[U](f: Nothing => U): OverloadResult[U] = End
    override def toList: scala.List[NonEmptyVector[Nothing]] = Nil
  }
  final case class List[+T](values: NonEmptyVector[T], next: OverloadResult[T]) extends OverloadResult[T] {

    override def map[U](f: T => U): OverloadResult.List[U] =
      List(values.map(f), next.map(f))

    override def toList: scala.List[NonEmptyVector[T]] =
      values :: next.toList

    def toNonEmptyList: NonEmptyList[NonEmptyVector[T]] =
      NonEmptyList(values, next.toList)
  }
}
