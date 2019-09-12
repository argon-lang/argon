package dev.argon.io

import dev.argon.stream._
import java.io.InputStream
import java.util.Objects

import cats._
import cats.implicits._
import cats.data.NonEmptyVector
import zio.blocking.Blocking
import zio._
import zio.stream.ZStream

object InputStreamReaderTransformation {

  @SuppressWarnings(Array(
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.Var",
  ))
  private final class TransformInputStream[R](getNext: () => Chunk[Byte]) extends InputStream {

    private var remainingData: Chunk[Byte] = Chunk.empty
    private var isEOF: Boolean = false

    override def read(): Int = {
      val buff = new Array[Byte](1)
      if(read(buff) < 0)
        -1
      else
        buff(0) & 0xFF
    }

    override def read(b: Array[Byte], off: Int, len: Int): Int = {
      Objects.checkFromIndexSize(off, len, b.length)
      if(len === 0)
        0
      else if(isEOF)
        -1
      else {
        (if(remainingData.isEmpty) getNext() else remainingData) match {
          case data if data.isEmpty =>
            isEOF = true
            -1
          case data =>
            val len2 = Math.min(len, data.length)
            Array.copy(data.toArray, 0, b, off, len2)

            remainingData = data.drop(len2)
            len2
        }
      }
    }
  }

  def apply[R, E, L[_, _], A](data: ZStream[R, E, Chunk[Byte]])(readHandler: InputStream => ZIO[R, E, A]): ZIO[R, E, A] =
    data.filterNot(_.isEmpty).process.use { pull =>
      for {
        runtime <- ZIO.runtime[R]
        inputStream <- IO.effectTotal {
          new TransformInputStream[R](() => runtime.unsafeRun(pull.foldCauseM(
            failure = cause => cause.failureOrCause match {
              case Left(None) =>  IO.succeed(Chunk.empty)
              case _ => IO.halt(cause)
            },
            success = IO.succeed
          )))
        }

        result <- readHandler(inputStream)
      } yield result
    }


}


