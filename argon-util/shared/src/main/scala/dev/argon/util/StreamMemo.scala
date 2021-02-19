package dev.argon.util

import zio._
import zio.stream.{ZStream, Stream}

import scala.reflect.ClassTag

object StreamMemo {

  def make[R, E, A](stream: ZStream[R, E, A]): URManaged[R, Stream[E, A]] = for {
    env <- ZManaged.environment[R]
    data <- ZManaged.fromEffect(MemoData.empty[E, A])
    dataLock <- Managed.fromEffect(Semaphore.make(1))
    process <- stream.process
  } yield ZStream(
    for {
      readCount <- Managed.fromEffect(Ref.make(0))
    } yield dataLock.withPermit(
      readCount.get.flatMap { prevRead =>
        data.values.get.flatMap { values =>
          if(prevRead < values.size)
            readCount.set(values.size).as(values.drop(prevRead))
          else
            data.result.get.flatMap {
              case Some(result) => IO.halt(result)
              case None =>
                readNext(data, process, readCount)
            }
        }
      }
    )
  ).provide(env)

  private def readNext[R, E, A](data: MemoData[E, A], process: ZIO[R, Option[E], Chunk[A]], readCount: Ref[Int]): ZIO[R, Option[E], Chunk[A]] =
    process.foldCauseM(
      failure = cause => data.result.set(Some(cause)) *> IO.halt(cause),
      success = chunk =>
        for {
          newCount <- data.values.modify { prev =>
            val newValues = prev ++ chunk
            (newValues.size, newValues)
          }
          _ <- readCount.set(newCount)
        } yield chunk
    )

  private final case class MemoData[E, A](values: Ref[Chunk[A]], result: Ref[Option[Cause[Option[E]]]])

  private object MemoData {

    def empty[E, A]: UIO[MemoData[E, A]] = for {
      valuesRef <- Ref.make[Chunk[A]](Chunk.empty)
      resultRef <- Ref.make[Option[Cause[Option[E]]]](None)
    } yield MemoData(valuesRef, resultRef)

  }

}
