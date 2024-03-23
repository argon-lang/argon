package dev.argon.util

import zio.*
import scala.reflect.TypeTest

object ZIOErrorUtil {

  def multiCause[E](head: E, tail: E*): Cause[E] =
    tail.foldLeft(Cause.fail(head))((cause, compError) => Cause.Both(cause, Cause.fail(compError)))

  def multiCauseChunk[E](chunk: NonEmptyChunk[E]): Cause[E] = multiCause(chunk.head, chunk.tail*)


  def splitCauseError[E0, E1](cause: Cause[E0 | E1])(using e0tt: TypeTest[E0 | E1, E0], e1tt: TypeTest[E0 | E1, E1]): (Cause[E0], Cause[E1]) =
    cause match {
      case cause @ (Cause.Empty | Cause.Die(_, _) | Cause.Interrupt(_, _)) => (cause, cause)
      case Cause.Fail(value: E0, trace) => (Cause.fail(value, trace), Cause.empty)
      case Cause.Fail(value: E1, trace) => (Cause.empty, Cause.fail(value, trace))
      case Cause.Stackless(cause, stackless) =>
        val (c0, c1) = splitCauseError[E0, E1](cause)
        (Cause.Stackless(c0, stackless), Cause.Stackless(c1, stackless))

      case Cause.Then(left, right) =>
        val (left0, left1) = splitCauseError[E0, E1](left)
        val (right0, right1) = splitCauseError[E0, E1](right)
        (Cause.Then(left0, right0), Cause.Then(left1, right1))

      case Cause.Both(left, right) =>
        val (left0, left1) = splitCauseError[E0, E1](left)
        val (right0, right1) = splitCauseError[E0, E1](right)
        (Cause.Both(left0, right0), Cause.Both(left1, right1))
    }

  def catchSplit[R, E0, E1, A, B >: A](a: ZIO[R, E0 | E1, A])(f: E0 => ZIO[R, E1, B])(using e0tt: TypeTest[E0 | E1, E0], e1tt: TypeTest[E0 | E1, E1]): ZIO[R, E1, B] =
    a.sandboxWith[R, E1, B](_.catchAll { cause =>
      val (c0, c1) = splitCauseError[E0, E1](cause)
      c0.failureOption match {
        case Some(e0) => f(e0).sandbox
        case None => ZIO.fail(c1)
      }
    })
      


}
