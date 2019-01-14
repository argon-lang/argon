package com.mi3software.argon.util.stream

import java.io._

import com.mi3software.argon.util.FileOperations
import scalaz._
import Scalaz._
import scalaz.effect.IO

object FileStream {

  def readFile(file: File, bufferSize: Int): ArStream[IO, Byte] = new ArStream[IO, Byte] {
    override def foldChunksM[B](start: B)(f: (B, Vector[Byte]) => IO[B])(implicit monadInstance: Monad[IO]): IO[B] =
      FileOperations.fileInputStream(file) { stream =>
        IO { new Array[Byte](bufferSize) }.flatMap { buffer =>

          def accum(b: B): IO[B] =
            IO { stream.read(buffer) }.flatMap {
              case bytesRead if bytesRead > 0 => f(b, buffer.toVector)
              case _ => b.point[IO]
            }

          accum(start)
        }
      }
  }

  def readFileText(file: File, bufferSize: Int): ArStream[IO, Char] = new ArStream[IO, Char] {
    override def foldChunksM[B](start: B)(f: (B, Vector[Char]) => IO[B])(implicit monadInstance: Monad[IO]): IO[B] =
      FileOperations.fileInputStream(file) { stream =>
        IO { new Array[Char](bufferSize) }.flatMap { buffer =>
          val reader = new InputStreamReader(stream)

          def accum(b: B): IO[B] =
            IO { reader.read(buffer) }.flatMap {
              case charsRead if charsRead > 0 => f(b, buffer.toVector)
              case _ => b.point[IO]
            }

          accum(start)
        }
      }
  }

  def writeFile(file: File, stream: ArStream[IO, Byte]): IO[Unit] =
    FileOperations.fileOutputStream(file) { outStream =>
      stream.foldChunksM(()) { (_, chunk) =>
        IO { outStream.write(chunk.toArray) }
      }
    }

  def writeFileText(file: File, stream: ArStream[IO, Byte]): IO[Unit] =
    FileOperations.fileOutputStream(file) { outStream =>
      IO { new OutputStreamWriter(outStream) }.flatMap { writer =>
        stream.foldChunksM(()) { (_, chunk) =>
          IO { outStream.write(chunk.toArray) }
        }
      }
    }

}
