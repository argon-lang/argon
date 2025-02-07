package dev.argon.util

import zio.*
import scala.reflect.TypeTest

object ErrorTestUtils {
  def splitCause[A, B](cause: Cause[A | B])(using TypeTest[A | B, A], TypeTest[A | B, B]): Either[A, Cause[B]] =
    cause match {
      case Cause.Fail(a: A, trace) => Left(a)
      case Cause.Fail(b: B, trace) => Right(Cause.Fail(b, trace))
      case Cause.Die(t, trace) => Right(Cause.Die(t, trace))
      case Cause.Interrupt(fiber, trace) => Right(Cause.Interrupt(fiber, trace))

      case Cause.Both(left, right) =>
        for
          left <- splitCause[A, B](left)
          right <- splitCause[A, B](right)
        yield Cause.Both(left, right)
      
      case Cause.Then(left, right) =>
        for
          left <- splitCause[A, B](left)
          right <- splitCause[A, B](right)
        yield Cause.Then(left, right)

      case Cause.Stackless(c, stackless) =>
        for
          c <- splitCause[A, B](c)
        yield Cause.Stackless(c, stackless)
        
      case Cause.Empty => Right(Cause.Empty)
    }
}
