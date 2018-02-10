package com.mi3software.argon.compiler

import com.mi3software.argon.util.{FileID, NamespacePath}

import scalaz._
import Scalaz._

sealed trait FuncDescriptor
object FuncDescriptor {
  @deriving(Equal)
  final case class InNamespace(module: ModuleDescriptor, namespace: NamespacePath, name: FunctionName) extends FuncDescriptor

  implicit val equalInstance: Equal[FuncDescriptor] = {
    case (a @ InNamespace(_, _, _), b @ InNamespace(_, _, _)) => a === b
  }
}

sealed trait FunctionName
object FunctionName {
  @deriving(Equal)
  final case class Normal(name: String) extends FunctionName

  @deriving(Equal)
  final case class Unnamed(fileId: FileID, index: Int) extends FunctionName

  implicit val equalInstance: Equal[FunctionName] = {
    case (a @ Normal(_), b @ Normal(_)) => a === b
    case (Normal(_), _) | (_, Normal(_)) => false

    case (a @ Unnamed(_, _), b @ Unnamed(_, _)) => a === b
  }
}

