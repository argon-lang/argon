package dev.argon.util

object AnyExtensions {
  implicit class AnyExtensionsImpl[A](private val a: A) extends AnyVal {

    def upcast[B >: A]: B = a

  }
}
