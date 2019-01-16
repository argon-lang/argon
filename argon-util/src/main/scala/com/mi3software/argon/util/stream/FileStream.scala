package com.mi3software.argon.util.stream

import java.io._

import com.mi3software.argon.util.{FileOperations, NonEmptyVector}
import scalaz._
import Scalaz._
import scalaz.zio._
import scalaz.zio.interop.scalaz72._

object FileStream {

  type MonadErrorThrowable[F[_, _]] = MonadError[F[Throwable, ?], Throwable]

  def readFile[F[_, _] : MonadErrorThrowable : ZIO](file: File, bufferSize: Int): ArStream[F[Throwable, ?], Byte, Unit] = new ArStream[F[Throwable, ?], Byte, Unit] {

    override def foldChunksM[B, R2](start: B)(resultHandler: (B, Unit) => R2)(f: (B, NonEmptyVector[Byte]) => F[Throwable, B])(implicit monadInstance: Monad[F[Throwable, ?]]): F[Throwable, R2] =
      FileOperations.fileInputStream(file) { stream =>
        ZIO[F].liftZIO(IO.syncThrowable { new Array[Byte](bufferSize) }).flatMap { buffer =>

          def accum(b: B): F[Throwable, B] =
            ZIO[F].liftZIO(IO.syncThrowable { stream.read(buffer) }).flatMap {
              case bytesRead if bytesRead > 0 =>
                buffer.iterator.take(bytesRead).toVector match {
                  case head +: tail => f(b, NonEmptyVector(head, tail))
                  case Vector() => b.point[F[Throwable, ?]]
                }
              case _ => b.point[F[Throwable, ?]]
            }

          accum(start)
        }
      }.map { b => resultHandler(b, ()) }
  }

  def readFileText[F[_, _] : MonadErrorThrowable : ZIO](file: File, bufferSize: Int): ArStream[F[Throwable, ?], Char, Unit] = new ArStream[F[Throwable, ?], Char, Unit] {

    override def foldChunksM[B, R2](start: B)(resultHandler: (B, Unit) => R2)(f: (B, NonEmptyVector[Char]) => F[Throwable, B])(implicit monadInstance: Monad[F[Throwable, ?]]): F[Throwable, R2] =
      FileOperations.fileReader(file) { reader =>
        ZIO[F].liftZIO(IO.syncThrowable { new Array[Char](bufferSize) }).flatMap { buffer =>

          def accum(b: B): F[Throwable, B] =
            ZIO[F].liftZIO(IO.syncThrowable { reader.read(buffer) }).flatMap {
              case charsRead if charsRead > 0 =>
                buffer.iterator.take(charsRead).toVector match {
                  case head +: tail => f(b, NonEmptyVector(head, tail))
                  case Vector() => b.point[F[Throwable, ?]]
                }
              case _ => b.point[F[Throwable, ?]]
            }

          accum(start)
        }
      }.map { b => resultHandler(b, ()) }
  }

  def writeFile(file: File, stream: ArStream[IO[Throwable, ?], Byte, Unit]): IO[Throwable, Unit] =
    FileOperations.fileOutputStream(file) { outStream =>
      stream.foldChunksM(()) { (_, _) => } { (_, chunk) =>
        IO.syncThrowable { outStream.write(chunk.toArray) }
      }
    }

  def writeFileText(file: File, stream: ArStream[IO[Throwable, ?], Char, Unit]): IO[Throwable, Unit] =
    FileOperations.filePrintWriter(file) { writer =>
      stream.foldChunksM(()) { (_, _) => } { (_, chunk) =>
        IO.syncThrowable { writer.write(chunk.toArray) }
      }
    }

}
