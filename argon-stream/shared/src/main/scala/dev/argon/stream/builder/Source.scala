package dev.argon.stream.builder

import cats._
import cats.arrow.FunctionK
import cats.data.{NonEmptyVector, StateT}
import cats.implicits._
import zio.stream.ZStream
import zio.{Exit, IO, Queue, Ref, ZIO}

trait Source[R, E, A] {

  def foreach(f: A => ZIO[R, E, Unit]): ZIO[R, E, Unit]

  def toZStream: ZStream[R, E, A] =
    ZStream.unwrap(
      for {
        queue <- Queue.bounded[Exit[Option[E], A]](1)

        _ <- foreach { value => queue.offer(Exit.Success(value)).unit }.foldCauseM(
          failure = cause => queue.offer(Exit.Failure(cause.map(Some.apply))).unit,
          success = _ => queue.offer(Exit.fail(None)).unit
        ).fork

      } yield ZStream.fromQueue(queue).collectWhileSuccess
    )


}
