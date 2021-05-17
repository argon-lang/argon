package dev.argon.util

import cats.data.{NonEmptyList, NonEmptyVector}
import zio.{Cause, NonEmptyChunk}


object ZIOErrorUtil {

  def multiCause[E](head: E, tail: E*): Cause[E] =
    tail.foldLeft(Cause.fail(head))((cause, compError) => Cause.Both(cause, Cause.fail(compError)))

  def multiCauseChunk[E](chunk: NonEmptyChunk[E]): Cause[E] =
    multiCause(chunk.head, chunk.tail: _*)

  def multiCauseVector[E](chunk: NonEmptyVector[E]): Cause[E] =
    multiCause(chunk.head, chunk.tail: _*)

  def multiCauseList[E](chunk: NonEmptyList[E]): Cause[E] =
    multiCause(chunk.head, chunk.tail: _*)

}
