package com.mi3software.argon.compiler.core

sealed trait Mutability
object Mutability {
  case object Mutable extends Mutability
  case object NonMutable extends Mutability

  def fromIsMutable(isMutable: Boolean): Mutability =
    if(isMutable)
      Mutable
    else
      NonMutable
}
