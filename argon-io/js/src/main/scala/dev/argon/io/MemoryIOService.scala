package dev.argon.io
import java.io.{ByteArrayOutputStream, FileNotFoundException, IOException}

import cats._
import cats.implicits._
import dev.argon.stream.builder._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import zio.stream._
import zio._

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.|

@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.ToString", "org.wartremover.warts.Null"))
class MemoryIOService(fileSystem: Ref[Map[String, Chunk[Byte]]]) extends FileIOServiceCommon {

  override def getEnv(name: String): UIO[Option[String]] = IO.succeed(None)

  private def readFileBytes(path: Path): IO[IOException, Chunk[Byte]] =
    fileSystem.get.flatMap { fs =>
      getAbsolutePath(path).flatMap { fileName =>
        IO.fromEither(fs.get(fileName.pathName).toRight { new FileNotFoundException(fileName.pathName) })
      }
    }


  override def readAllText(path: Path): IO[IOException, String] =
    readFileBytes(path).flatMap { data =>
      IO.effectTotal {
        val decoder = new JSStringDecoder("utf8")
        decoder.end(new Uint8Array(data.toArray.toJSArray))
      }
    }


  override def readText[E](errorHandler: IOException => E)(path: Path): Stream[E, Char] =
    ZStream.fromEffect(readAllText(path))
      .mapError(errorHandler)
      .flatMap(ZStream.fromIterable(_))

  override def writeToFile[R, E, X](errorHandler: IOException => E)(path: Path)(data: Source[ZIO[R, E, *], Chunk[Byte], X]): ZIO[R, E, X] = for {
    fileName <- getAbsolutePath(path).mapError(errorHandler)
    outputStream <- IO.effectTotal { new ByteArrayOutputStream() }
    x <- data.foreach { chunk =>
      IO.effectTotal { outputStream.write(chunk.toArray) }
    }
    content <- IO.effectTotal { Chunk.fromArray(outputStream.toByteArray) }
    _ <- fileSystem.update { fs => fs.updated(fileName.pathName, content) }
  } yield x


  override def isDirectory(path: Path): IO[IOException, Boolean] =
    getAbsolutePath(path).flatMap { fileName =>
      val fileNameStr = fileName.pathName + "/"
      fileSystem.get.map { fs =>
        fs.exists { case (name, _) => name.startsWith(fileNameStr) }
      }
    }

  override def listDirectory(path: Path): Stream[IOException, Path] =
    ZStream.flatten(
      ZStream.fromEffect(
        getAbsolutePath(path).flatMap { fileName =>
          val fileNameStr = fileName.pathName + "/"
          fileSystem.get.map { fs =>
            ZStream.fromIterable(
              fs.keys
                .filter { name => name.startsWith(fileNameStr) }
                .map { name =>
                  val restName = name.substring(fileNameStr.length)
                  val slash = restName.indexOf("/")

                  val nextPart =
                    if(slash < 0)
                      restName
                    else
                      restName.substring(0, slash)

                  new Path(JSPath.join(path.pathName, nextPart))
                }
                .toSet
                .toSeq
            )
          }
        }
      )
    )

  override def openZipFile[R, E](errorHandler: IOException => E)(path: Path): Managed[E, ZipFileReader[ZIO[R, E, *]]] =
    ZManaged.fromEffect(
      readFileBytes(path)
        .mapError(errorHandler)
        .flatMap { data =>
          promiseToIO(errorHandler)(new JSZip().loadAsync(new Uint8Array(data.toArray.toJSArray)))
        }
        .map { zip =>
          new ZipFileReader[ZIO[R, E, *]] {
            override def getEntryStream(name: String): Source[ZIO[R, E, *], Chunk[Byte], Unit] =
              ZStreamSource(
                ZStream.flatten(
                  ZStream.fromEffect(
                    promiseToIO(errorHandler)(zip.file(name).async("uint8array"))
                      .map { data => ZStream(Chunk.fromArray(data.toArray.map { _.toByte })) }
                  )
                )
              )

          }
        }
    )


}
