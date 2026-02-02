package dev.argon.expr

sealed trait ErasureMode derives CanEqual {
  import ErasureMode.*

  def asTypeAnnotation: ErasureMode =
    this match {
      case ErasureMode.Erased => ErasureMode.Erased
      case ErasureMode.Token | ErasureMode.TypeAnnotationToken => ErasureMode.TypeAnnotationToken
      case ErasureMode.Concrete | ErasureMode.TypeAnnotationConcrete => ErasureMode.TypeAnnotationConcrete
    }
  
  def forSubExpression(inner: ErasureMode): ErasureMode =
    if this == Erased then Erased else inner
    
}

object ErasureMode {
  sealed trait Declared extends ErasureMode
  sealed trait DeclaredNonToken extends Declared
  
  case object Erased extends DeclaredNonToken
  case object TypeAnnotationToken extends ErasureMode
  case object Token extends Declared
  case object TypeAnnotationConcrete extends ErasureMode
  case object Concrete extends DeclaredNonToken
}
