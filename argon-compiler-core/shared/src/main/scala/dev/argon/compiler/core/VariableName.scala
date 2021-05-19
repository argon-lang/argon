package dev.argon.compiler.core

import cats._
import cats.implicits._
import dev.argon.util.DeriveHelpers

sealed trait VariableName
object VariableName {
  final case class Normal(name: String) extends VariableName
  case object Unnamed extends VariableName

  implicit val eqInstance: Eq[VariableName] = DeriveHelpers.eq

}