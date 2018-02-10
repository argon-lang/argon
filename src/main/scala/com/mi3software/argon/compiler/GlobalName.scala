package com.mi3software.argon.compiler

import com.mi3software.argon.util.FileID

import scalaz._
import Scalaz._

sealed trait GlobalName
object GlobalName {
  @deriving(Equal)
  final case class Normal(name: String) extends GlobalName

  @deriving(Equal)
  final case class Unnamed(fileId: FileID, index: Int) extends GlobalName

  implicit val equalInstance: Equal[GlobalName] = {
    case (a @ Normal(_), b @ Normal(_)) => a === b
    case (Normal(_), _) | (_, Normal(_)) => false

    case (a @ Unnamed(_, _), b @ Unnamed(_, _)) => a === b
  }
}