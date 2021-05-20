package dev.argon.compiler.core

import cats._
import cats.implicits._
import dev.argon.parser
import dev.argon.util.DeriveHelpers

sealed trait MemberName

object MemberName {

  final case class Normal(name: String) extends MethodName
  object Normal {
    implicit val eqInstance: Eq[Normal] = DeriveHelpers.eq
  }
  final case class Mutator(name: String) extends MethodName
  object Mutator {
    implicit val eqInstance: Eq[Mutator] = DeriveHelpers.eq
  }

  case object Unnamed extends MethodName {
    implicit val eqInstance: Eq[Unnamed.type] = DeriveHelpers.eq
  }

  case object Call extends MethodName {
    implicit val eqInstance: Eq[Call.type] = DeriveHelpers.eq
  }
  case object New extends MemberName{
    implicit val eqInstance: Eq[New.type] = DeriveHelpers.eq
  }

  implicit val eqInstance: Eq[MemberName] = {
    case (a: MethodName, b: MethodName) => a === b
    case (New, New) => true
    case _ => false
  }

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