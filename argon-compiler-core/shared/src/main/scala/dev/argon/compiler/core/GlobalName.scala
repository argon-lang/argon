package dev.argon.compiler.core

import cats._
import cats.implicits._

sealed trait GlobalName
object GlobalName {
  sealed trait NonEmpty extends GlobalName

  final case class Normal(name: String) extends NonEmpty
  final case class Operator(name: String) extends NonEmpty

  case object Unnamed extends GlobalName

  implicit val eqInstance: Eq[GlobalName] = cats.derived.semiauto.eq
}