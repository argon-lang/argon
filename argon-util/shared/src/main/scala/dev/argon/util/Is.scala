package dev.argon.util

import scala.annotation.unused

sealed trait Is[A, B]:
  def convert(a: A): B
  def flip: B Is A
  def substitute[F[_]](fa: F[A]): F[B]
  def substituteBounded[L >: A | B, U <: L & A & B, F[_ >: U <: L]](fa: F[A]): F[B]
  def both(a: A): A & B
end Is

object Is:

  given refl[A]: Is[A, A] with
    def convert(a: A): A = a
    def flip: A Is A = this
    def substitute[F[_]](fa: F[A]): F[A] = fa
    def substituteBounded[L >: A | A, U <: L & A & A, F[_ >: U <: L]](fa: F[A]): F[A] = fa
    def both(a: A): A & A = a
  end refl

  def nothing[A](a: A)(using aNothing: A Is Nothing): Nothing = aNothing.convert(a)
end Is
