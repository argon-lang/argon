package dev.argon.util

extension [A](self: A) {
  def upcast[B >: A]: B = self
}

extension [F[+_], A](self: F[A]) {
  def widen[B >: A]: F[B] = self
}
