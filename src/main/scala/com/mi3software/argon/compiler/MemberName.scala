package com.mi3software.argon.compiler

import scalaz._
import Scalaz._

sealed trait MemberName
object MemberName {

  @deriving(Equal)
  final case class Normal(name: String) extends MemberName

  @deriving(Equal)
  final case class Unnamed(index: Int) extends MemberName

  final case object Call extends MemberName

  implicit val equalInstance: Equal[MemberName] = {
    case (a @ Normal(_), b @ Normal(_)) => a === b
    case (Normal(_), _) | (_, Normal(_)) => false

    case (a @ Unnamed(_), b @ Unnamed(_)) => a === b
    case (Unnamed(_), _) | (_, Unnamed(_)) => false

    case (Call, Call) => true
  }
}