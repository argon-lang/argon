package dev.argon.compiler.core

import cats._
import cats.implicits._
import dev.argon.util.DeriveHelpers

sealed trait GlobalName
object GlobalName {
  sealed trait NonEmpty extends GlobalName

  final case class Normal(name: String) extends NonEmpty
  final case class Operator(name: String) extends NonEmpty

  case object Unnamed extends GlobalName

  implicit val eqInstance: Eq[GlobalName] = {
    case (Normal(a), Normal(b)) => a === b
    case (Operator(a), Operator(b)) => a === b
    case (Unnamed, Unnamed) => true
    case _ => false
  }
}