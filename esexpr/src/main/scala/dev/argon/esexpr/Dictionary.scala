package dev.argon.esexpr

final case class Dictionary[+A](@dict dict: Map[String, A]) derives CanEqual, ESExprCodec
