package com.mi3software.argon.compiler.lookup


sealed trait OverloadResult[+T] {
  def map[U](f: T => U): OverloadResult[U]
}
object OverloadResult {
  case object End extends OverloadResult[Nothing] {
    override def map[U](f: Nothing => U): OverloadResult[U] = End
  }
  final case class List[+T](values: Vector[T], next: OverloadResult[T]) extends OverloadResult[T] {

    override def map[U](f: T => U): OverloadResult[U] =
      List(values.map(f), next.map(f))
  }
}
