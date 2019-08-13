package dev.argon.io

import dev.argon.stream._
import java.io.InputStream
import java.util.Objects

import cats.data.NonEmptyVector
import zio.blocking.Blocking
import zio._

trait InputStreamReaderTransformation[-R, +E, +X] extends StreamTransformation[ZIO, R, E, Byte, Unit, Nothing, X] {

  def readDirectly[R2 <: R, E2 >: E](inputStream: InputStream): ZIO[R2, E2, X]

}

object InputStreamReaderTransformation {

  @SuppressWarnings(Array(
    "org.wartremover.warts.Equals",
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.Var",
  ))
  private final class TransformInputStream[R](getNext: () => Vector[Byte]) extends InputStream {

    private var remainingData: Vector[Byte] = Vector.empty

    override def read(): Int = {
      val buff = new Array[Byte](1)
      if(read(buff) < 0)
        -1
      else
        buff(0) & 0xFF
    }

    override def read(b: Array[Byte], off: Int, len: Int): Int = {
      Objects.checkFromIndexSize(off, len, b.length)
      if(len == 0)
        0
      else {
        (if(remainingData.nonEmpty) remainingData else getNext()) match {
          case Vector() => -1
          case data =>
            data.copyToArray(b, off, len)

            remainingData = data.drop(len)
            Math.min(len, data.size)
        }
      }
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.Null"))
  def apply[R, E, X](readHandler: InputStream => ZIO[R, E, X]): InputStreamReaderTransformation[R, E, X] =
    new ProducerTransformation[R, E, Byte, X] with InputStreamReaderTransformation[R, E, X] {
      override def readDirectly[R2 <: R, E2 >: E](inputStream: InputStream): ZIO[R2, E2, X] =
        readHandler(inputStream)

      override protected def consumerHandler(getNext: IO[E, Vector[Byte]]): ZIO[R, E, X] =
        ZIO.runtime[R].flatMap { runtime =>
          IO.effectTotal { new TransformInputStream[R](() => runtime.unsafeRun(getNext)) }
            .flatMap(readHandler)
        }
    }

}


