package dev.argon.grammar

import cats._
import cats.implicits._

import scala.reflect.ClassTag

trait TokenMatcher[T, TParsed] {
  def matchToken(other: T): Option[TParsed]
}

object TokenMatcher {
  final case class Value[T: Eq](value: T) extends TokenMatcher[T, T] {
    override def matchToken(other: T): Option[T] =
      if(value === other)
        Some(other)
      else
        None
  }

  final case class Subtype[T, TParsed <: T](tag: ClassTag[TParsed]) extends TokenMatcher[T, TParsed] {
    override def matchToken(other: T): Option[TParsed] = {
      implicit val tag2 = tag
      other match {
        case value: TParsed => Some(value)
        case _ => None
      }
    }
  }

  final case class Anything[T, TParsed](f: T => Option[TParsed]) extends TokenMatcher[T, TParsed] {
    override def matchToken(other: T): Option[TParsed] =
      f(other)
  }
}
