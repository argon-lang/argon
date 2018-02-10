package com.mi3software.argon.compiler

sealed trait Mutability
object Mutability {
  case object Mutable extends Mutability
  case object NonMutable extends Mutability
}
