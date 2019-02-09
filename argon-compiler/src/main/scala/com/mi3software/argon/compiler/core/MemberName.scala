package com.mi3software.argon.compiler.core

import scalaz.Scalaz._
import scalaz._

sealed trait MemberName
sealed trait MethodName extends MemberName
object MemberName {

  @deriving(Equal)
  final case class Normal(name: String) extends MethodName

  case object Unnamed extends MethodName

  case object Call extends MethodName
  case object New extends MemberName

  implicit val equalInstance: Equal[MemberName] = {
    case (New, New) => true
    case (New, _) | (_, New) => false

    case (a: MethodName, b: MethodName) => a === b
  }
}

object MethodName {
  import MemberName._

  implicit val equalInstance: Equal[MethodName] = {
    case (a @ Normal(_), b @ Normal(_)) => a === b
    case (Normal(_), _) | (_, Normal(_)) => false

    case (Unnamed, Unnamed) => true
    case (Unnamed, _) | (_, Unnamed) => false

    case (Call, Call) => true
  }
}