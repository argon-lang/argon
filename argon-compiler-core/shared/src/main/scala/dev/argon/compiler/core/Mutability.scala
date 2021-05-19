package dev.argon.compiler.core

sealed trait Mutability
object Mutability {


  case object Mutable extends Mutability
  case object NonMutable extends Mutability

  def fromIsMutable(isMutable: Boolean): Mutability =
    if(isMutable)
      Mutable
    else
      NonMutable

  def toIsMutable(mutability: Mutability): Boolean =
    mutability match {
      case Mutable => true
      case NonMutable => false
    }
}
