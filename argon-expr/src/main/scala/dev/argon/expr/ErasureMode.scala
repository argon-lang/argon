package dev.argon.expr

enum ErasureMode derives CanEqual {
  case Erased
  case Concrete
  case TypeAnnotation // Concrete, but can be wrapped with erased

  def || (other: => ErasureMode): ErasureMode =
    this match
      case Erased => Erased
      case Concrete => other
      case TypeAnnotation =>
        other match
          case Erased => Erased
          case Concrete | TypeAnnotation => TypeAnnotation
        end match
    end match
}

object ErasureMode {
  def fromBoolean(b: Boolean): ErasureMode =
    if b then ErasureMode.Erased
    else ErasureMode.Concrete
}
