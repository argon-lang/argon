package com.mi3software.argon.util

import scalaz.NonEmptyList

object NonEmptyListExtensions {
  implicit class NonEmptyListExtensionsImpl[T](val l: NonEmptyList[T]) extends AnyVal {

    def reduce(f: (T, T) => T): T =
      l.tail.foldLeft(l.head)(f)

  }
}
