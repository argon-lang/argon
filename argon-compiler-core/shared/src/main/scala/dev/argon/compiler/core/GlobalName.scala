package dev.argon.compiler.core

import cats._
import cats.implicits._

sealed trait GlobalName
object GlobalName {
  final case class Normal(name: String) extends GlobalName
  final case class Operator(name: String) extends GlobalName

  case object Unnamed extends GlobalName

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit val eqInstance: Eq[GlobalName] = cats.derived.semi.eq
}