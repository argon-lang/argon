package dev.argon.compiler.core

import cats._
import cats.implicits._
import dev.argon.parser
import dev.argon.util.DeriveHelpers

sealed trait MemberName

object MemberName {

  final case class Normal(name: String) extends MethodName
  final case class Mutator(name: String) extends MethodName

  case object Unnamed extends MethodName

  case object Call extends MethodName
  case object New extends MemberName

  implicit val eqInstance: Eq[MemberName] = DeriveHelpers.eq

  def fromMethodNameSpecifier(specifier: parser.MethodNameSpecifier): MemberName =
    MethodName.fromMethodNameSpecifier(specifier)

}

sealed trait MethodName extends MemberName
object MethodName {
  implicit val eqInstance: Eq[MethodName] = DeriveHelpers.eq

  def fromMethodNameSpecifier(specifier: parser.MethodNameSpecifier): MethodName =
    specifier match {
      case parser.MethodNameSpecifier.Unnamed => MemberName.Unnamed
      case parser.MethodNameSpecifier.Named(name) => MemberName.Normal(name)
      case parser.MethodNameSpecifier.Mutator(name) => MemberName.Mutator(name)

    }
}