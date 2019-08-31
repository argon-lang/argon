package dev.argon.compiler.core

import cats._
import cats.implicits._
import dev.argon.parser

sealed trait MemberName

object MemberName {

  final case class Normal(name: String) extends MethodName
  final case class Mutator(name: String) extends MethodName

  case object Unnamed extends MethodName

  case object Call extends MethodName
  case object New extends MemberName

  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit val eqInstance: Eq[MemberName] = cats.derived.semi.eq

  def fromMethodNameSpecifier(specifier: parser.MethodNameSpecifier): MemberName =
    MethodName.fromMethodNameSpecifier(specifier)

}

sealed trait MethodName extends MemberName
object MethodName {
  @SuppressWarnings(Array("org.wartremover.warts.Equals"))
  implicit val eqInstance: Eq[MethodName] = cats.derived.semi.eq

  def fromMethodNameSpecifier(specifier: parser.MethodNameSpecifier): MethodName =
    specifier match {
      case parser.MethodNameSpecifier.Unnamed => MemberName.Unnamed
      case parser.MethodNameSpecifier.Named(name) => MemberName.Normal(name)
      case parser.MethodNameSpecifier.Mutator(name) => MemberName.Mutator(name)

    }
}