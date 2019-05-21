package dev.argon.util.stream

import java.io._

import dev.argon.util.{FileOperations, NonEmptyVector}
import scalaz._
import Scalaz._
import scalaz.zio._
import scalaz.zio.interop.scalaz72._

object FileStream {

  type MonadErrorThrowable[F[_, _]] = MonadError[F[Throwable, ?], Throwable]

  def readFile(file: File, bufferSize: Int): ArStream[Task, Byte, Unit] = new ArStream[Task, Byte, Unit] {

    override def foldChunksM[B, R2](start: B)(resultHandler: (B, Unit) => R2)(f: (B, NonEmptyVector[Byte]) => Task[B])(implicit monadInstance: Monad[Task]): Task[R2] =
      FileOperations.fileInputStream[Throwable, B](file) { stream =>
        IO.effect { new Array[Byte](bufferSize) }.flatMap { buffer =>

          def accum(b: B): Task[B] =
            IO.effect { stream.read(buffer) }.flatMap {
              case bytesRead if bytesRead > 0 =>
                buffer.iterator.take(bytesRead).toVector match {
                  case head +: tail => f(b, NonEmptyVector(head, tail))
                  case Vector() => b.point[Task]
                }
              case _ => b.point[Task]
            }

          accum(start)
        }
      }.map { b => resultHandler(b, ()) }
  }

  def readFileText[E](file: File, bufferSize: Int)(refineErr: PartialFunction[Throwable, E]): ArStream[IO[E, ?], Char, Unit] = new ArStream[IO[E, ?], Char, Unit] {

    override def foldChunksM[B, R2](start: B)(resultHandler: (B, Unit) => R2)(f: (B, NonEmptyVector[Char]) => IO[E, B])(implicit monadInstance: Monad[IO[E, ?]]): IO[E, R2] =
      FileOperations.fileReader[Throwable, Either[E, B]](file) { reader =>
        IO.effect { new Array[Char](bufferSize) }.flatMap { buffer =>

          def accum(b: B): IO[E, B] =
            IO.effect { reader.read(buffer) }.refineOrDie(refineErr).flatMap {
              case charsRead if charsRead > 0 =>
                buffer.iterator.take(charsRead).toVector match {
                  case head +: tail => f(b, NonEmptyVector(head, tail))
                  case Vector() => monadInstance.pure(b)
                }
              case _ => monadInstance.pure(b)
            }

          accum(start).either
        }
      }
        .refineOrDie(refineErr)
        .flatMap(IO.fromEither(_))
        .map { b => resultHandler(b, ()) }
  }

  def writeFile(file: File, stream: ArStream[IO[Throwable, ?], Byte, Unit]): IO[Throwable, Unit] =
    FileOperations.fileOutputStream[Throwable, Unit](file) { outStream =>
      stream.foldChunksM(()) { (_, _) => } { (_, chunk) =>
        IO.effect { outStream.write(chunk.toArray) }
      }
    }

  def writeFileText(file: File, stream: ArStream[IO[Throwable, ?], Char, Unit]): IO[Throwable, Unit] =
    FileOperations.filePrintWriter[Throwable, Unit](file) { writer =>
      stream.foldChunksM(()) { (_, _) => } { (_, chunk) =>
        IO.effect { writer.write(chunk.toArray) }
      }
    }

}
