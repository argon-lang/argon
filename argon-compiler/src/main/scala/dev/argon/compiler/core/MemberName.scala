package dev.argon.compiler.core

import cats._
import cats.implicits._

sealed trait MemberName

object MemberName {

  final case class Normal(name: String) extends MethodName

  case object Unnamed extends MethodName

  case object Call extends MethodName
  case object New extends MemberName

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit val eqInstance: Eq[MemberName] = cats.derived.semi.eq
}

sealed trait MethodName extends MemberName
object MethodName {
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit val eqInstance: Eq[MethodName] = cats.derived.semi.eq
}