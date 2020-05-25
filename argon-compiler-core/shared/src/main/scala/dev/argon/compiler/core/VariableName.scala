package dev.argon.compiler.core

import cats._
import cats.implicits._

sealed trait VariableName
object VariableName {
  final case class Normal(name: String) extends VariableName
  case object Unnamed extends VariableName

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit val eqInstance: Eq[VariableName] = cats.derived.semi.eq

}