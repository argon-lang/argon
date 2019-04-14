package com.mi3software.argon.compiler.core

import scalaz.Scalaz._
import scalaz._

@deriving(Equal)
sealed trait MemberName

@deriving(Equal)
sealed trait MethodName extends MemberName

object MemberName {

  final case class Normal(name: String) extends MethodName

  case object Unnamed extends MethodName

  case object Call extends MethodName
  case object New extends MemberName
}