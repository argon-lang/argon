package dev.argon.plugin;

import java.io.{InputStream, FilterInputStream}
import zio.*
import zio.stream.*
import dev.argon.util.ErrorWrapper
import dev.argon.util.JavaExecuteIO
import java.io.IOException


private[plugin] object JavaInputStreamWrap {

  def fromZStream[R, E >: IOException, EX <: Exception](stream: ZStream[R, E, Byte])(using Runtime[R], ErrorWrapper[E, EX]): InputStream =
    val reservation = JavaExecuteIO.runInterruptable(stream.channel.toPull.reserve)

    val pull = JavaExecuteIO.runInterruptable(reservation.acquire)

    new InputStream {

      private var chunk: Chunk[Byte] = Chunk.empty
      private var index: Int = 0

      override def read(): Int =
        val b = new Array[Byte](1)
        if read(b) == 0 then -1
        else b(0)
      end read

      override def read(b: Array[Byte], off: Int, len: Int): Int =
        while index >= chunk.size do
          val exit = JavaExecuteIO.runInterruptable(pull)
          exit match {
            case Left(_) => return 0
            case Right(chunk) =>
              this.chunk = chunk
              index = 0
          }
        end while

        val bytesRead = len.min(chunk.size - index)
        for i <- 0 until bytesRead do
          b(off + i) = chunk.byte(index + i)
        
        index += bytesRead
        bytesRead
      end read

      override def close(): Unit =
        super.close()
        JavaExecuteIO.runInterruptable(reservation.release(Exit.unit))
      end close
    }
  end fromZStream

}
