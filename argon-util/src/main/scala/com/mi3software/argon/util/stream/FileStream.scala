package com.mi3software.argon.util.stream

import java.io._

import com.mi3software.argon.util.{FileOperations, NonEmptyVector}
import scalaz._
import Scalaz._
import scalaz.effect.{IO, LiftIO}

object FileStream {

  type MonadErrorThrowable[F[_]] = MonadError[F, Throwable]

  def readFile[F[_] : MonadErrorThrowable : LiftIO](file: File, bufferSize: Int): ArStream[F, Byte, Unit] = new ArStream[F, Byte, Unit] {

    override def foldChunksM[B, R2](start: B)(resultHandler: (B, Unit) => R2)(f: (B, NonEmptyVector[Byte]) => F[B])(implicit monadInstance: Monad[F]): F[R2] =
      FileOperations.fileInputStream(file) { stream =>
        LiftIO[F].liftIO(IO { new Array[Byte](bufferSize) }).flatMap { buffer =>

          def accum(b: B): F[B] =
            LiftIO[F].liftIO(IO { stream.read(buffer) }).flatMap {
              case bytesRead if bytesRead > 0 =>
                buffer.iterator.take(bytesRead).toVector match {
                  case head +: tail => f(b, NonEmptyVector(head, tail))
                  case Vector() => b.point[F]
                }
              case _ => b.point[F]
            }

          accum(start)
        }
      }.map { b => resultHandler(b, ()) }
  }

  def readFileText[F[_] : MonadErrorThrowable : LiftIO](file: File, bufferSize: Int): ArStream[F, Char, Unit] = new ArStream[F, Char, Unit] {

    override def foldChunksM[B, R2](start: B)(resultHandler: (B, Unit) => R2)(f: (B, NonEmptyVector[Char]) => F[B])(implicit monadInstance: Monad[F]): F[R2] =
      FileOperations.fileReader(file) { reader =>
        LiftIO[F].liftIO(IO { new Array[Char](bufferSize) }).flatMap { buffer =>

          def accum(b: B): F[B] =
            LiftIO[F].liftIO(IO { reader.read(buffer) }).flatMap {
              case charsRead if charsRead > 0 =>
                buffer.iterator.take(charsRead).toVector match {
                  case head +: tail => f(b, NonEmptyVector(head, tail))
                  case Vector() => b.point[F]
                }
              case _ => b.point[F]
            }

          accum(start)
        }
      }.map { b => resultHandler(b, ()) }
  }

  def writeFile(file: File, stream: ArStream[IO, Byte, Unit]): IO[Unit] =
    FileOperations.fileOutputStream(file) { outStream =>
      stream.foldChunksM(()) { (_, _) => } { (_, chunk) =>
        IO { outStream.write(chunk.toArray) }
      }
    }

  def writeFileText(file: File, stream: ArStream[IO, Byte, Unit]): IO[Unit] =
    FileOperations.fileOutputStream(file) { outStream =>
      IO { new OutputStreamWriter(outStream) }.flatMap { writer =>
        stream.foldChunksM(()) { (_, _) => } { (_, chunk) =>
          IO { outStream.write(chunk.toArray) }
        }
      }
    }

}
