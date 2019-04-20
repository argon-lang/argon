package dev.argon.util

object AnyExtensions {
  implicit class AnyExtensionsImpl[A](val a: A) extends AnyVal {

    def upcast[B >: A]: B = a

  }
}
