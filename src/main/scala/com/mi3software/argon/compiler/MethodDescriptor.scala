package com.mi3software.argon.compiler

import scalaz._
import Scalaz._

@deriving(Equal)
final case class MethodDescriptor(typeDescriptor: ClassLikeDescriptor, name: String)

sealed trait MethodName
object MethodName {

  @deriving(Equal)
  final case class Normal(name: String) extends MethodName

  @deriving(Equal)
  final case class Unnamed(index: Int) extends MethodName

  final case object Call extends MethodName

  implicit val equalInstance: Equal[MethodName] = {
    case (a @ Normal(_), b @ Normal(_)) => a === b
    case (Normal(_), _) | (_, Normal(_)) => false

    case (a @ Unnamed(_), b @ Unnamed(_)) => a === b
    case (Unnamed(_), _) | (_, Unnamed(_)) => false

    case (Call, Call) => true
  }
}
