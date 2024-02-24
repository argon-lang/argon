package dev.argon.util


extension[T](x: T | Null)
  inline def toOption: Option[T] =
    if x == null then None else Some(x)

