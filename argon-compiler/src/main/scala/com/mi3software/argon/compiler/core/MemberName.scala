package com.mi3software.argon.compiler.core

import scalaz.Scalaz._
import scalaz._

sealed trait MemberName
object MemberName {

  @deriving(Equal)
  final case class Normal(name: String) extends MemberName

  @deriving(Equal)
  final case class Unnamed(index: Int) extends MemberName

  final case object Call extends MemberName
  final case object New extends MemberName

  implicit val equalInstance: Equal[MemberName] = {
    case (a @ Normal(_), b @ Normal(_)) => a === b
    case (Normal(_), _) | (_, Normal(_)) => false

    case (a @ Unnamed(_), b @ Unnamed(_)) => a === b
    case (Unnamed(_), _) | (_, Unnamed(_)) => false

    case (Call, Call) => true
    case (Call, _) | (_, Call) => false

    case (New, New) => true
  }
}