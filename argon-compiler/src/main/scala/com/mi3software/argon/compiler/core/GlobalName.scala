package com.mi3software.argon.compiler.core

import com.mi3software.argon.util.FileID
import scalaz.Scalaz._
import scalaz._

sealed trait GlobalName
object GlobalName {
  @deriving(Equal)
  final case class Normal(name: String) extends GlobalName

  @deriving(Equal)
  case object Unnamed extends GlobalName

  implicit val equalInstance: Equal[GlobalName] = {
    case (a @ Normal(_), b @ Normal(_)) => a === b
    case (Normal(_), _) | (_, Normal(_)) => false

    case (a @ Unnamed, b @ Unnamed) => a === b
  }
}