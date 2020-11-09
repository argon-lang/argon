package dev.argon.util

import zio._
import zio.stream.{ZStream, Stream}

import scala.reflect.ClassTag

@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.Null", "org.wartremover.warts.Var", "dev.argon.warts.ZioEffect"))
object StreamMemo {

  def make[R, E >: Null, A: ClassTag](stream: ZStream[R, E, A]): URManaged[R, Stream[E, A]] = for {
    env <- ZManaged.environment[R]
    data <- Managed.effectTotal { new MemoData[E, A]() }
    dataLock <- Managed.fromEffect(Semaphore.make(1))
    process <- stream.process
  } yield ZStream(
    for {
      readCount <- Managed.fromEffect(Ref.make(0))
    } yield
      readCount.get.flatMap { prevRead =>
        IO.effectTotal { data.valueCount }.flatMap { staleCount =>
          if(prevRead < staleCount)
            readCount.set(staleCount) *>
              getNewDataChunk(data, prevRead = prevRead, count = staleCount)
          else {
            dataLock.withPermit(
              IO.effectTotal { data.valueCount }.flatMap { count =>
                if(prevRead < count)
                  readCount.set(count) *>
                    getNewDataChunk(data, prevRead = prevRead, count = count)
                else
                  IO.effectTotal { data.result }.flatMap { result =>
                    if(result ne null)
                      IO.halt(result)
                    else
                      readNext(data, process) <*
                        IO.effectTotal { data.valueCount }.flatMap(readCount.set)
                  }
              }
            )
          }
        }
      }
  ).provide(env)

  private def getNewDataChunk[A: ClassTag](data: MemoData[_, A], prevRead: Int, count: Int): UIO[Chunk[A]] =
    IO.effectTotal {
      val arr = new Array[A](count - prevRead)
      val _ = System.arraycopy(data.values, prevRead, arr, 0, arr.length)
      Chunk.fromArray(arr)
    }

  private def readNext[R, E >: Null, A: ClassTag](data: MemoData[E, A], process: ZIO[R, Option[E], Chunk[A]]): ZIO[R, Option[E], Chunk[A]] =
    process.foldCauseM(
      failure = cause => IO.effectTotal { data.result = cause } *> IO.halt(cause),
      success = chunk => IO.effectTotal {
        val count = data.valueCount
        if(data.values.length - count < chunk.size) {
          val newArr = new Array[A](Math.max(2 * data.values.length, count + chunk.size))
          val _ = System.arraycopy(data.values, 0, newArr, 0, count)
          data.values = newArr
        }

        { val _ = chunk.copyToArray(data.values, count, chunk.size) }
        data.valueCount = count + chunk.size

        chunk
      }
    )


  private class MemoData[E >: Null, A: ClassTag] {
    @volatile
    var values = new Array[A](16)

    @volatile
    var valueCount = 0

    var result: Cause[Option[E]] = null
  }
}
