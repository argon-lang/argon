package dev.argon.io

import java.io.{FileReader, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.zip.ZipFile

import dev.argon.stream._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import zio._
import zio.stream._
import zio.blocking.Blocking
import zio.console.Console
import zio.system.System

import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._

class IOEnvironment(otherEnv: Blocking with Console with System) extends FileIO with Blocking with Console with System {
  override val fileIO: FileIO.Service = new FileIO.Service {

    override def getAbsolutePath(path: Path): IO[IOException, Path] =
      blocking.effectBlocking { path.toAbsolutePath }
        .refineOrDie { case e: IOException => e }

    override def readAllText(path: Path): IO[IOException, String] =
      blocking.effectBlocking { Files.readString(path, StandardCharsets.UTF_8) }
        .refineOrDie { case e: IOException => e }

    override def readText[E](errorHandler: IOException => E)(path: Path): Stream[E, Char] =
      stream.ZStream.managed(
        ZManaged.fromAutoCloseable(
          blocking.effectBlocking { Files.newBufferedReader(path, StandardCharsets.UTF_8) }
            .refineOrDie {
              case ex: IOException => errorHandler(ex)
            }
        )
      ).flatMap { reader =>
        stream.ZStream.unfoldM(()) { _ =>
          blocking.effectBlocking {
            val b = reader.read()
            if(b < 0)
              None
            else
              Some((b.toChar, ()))
          }
            .refineOrDie {
              case ex: IOException => errorHandler(ex)
            }
        }
      }

    override def fileOutputTransformation[E](errorHandler: IOException => E)(path: Path): Managed[E, StreamTransformation[ZIO, Any, E, Byte, Unit, Nothing, Unit]] =
      ZManaged.fromAutoCloseable(
        blocking.effectBlocking { Files.newOutputStream(path, StandardOpenOption.CREATE) }
      )
        .refineOrDie {
          case ex: IOException => errorHandler(ex)
        }
        .map { outputStream => OutputStreamTransformation(errorHandler, blocking)(outputStream) }

    override def isDirectory(path: Path): IO[IOException, Boolean] =
      blocking.effectBlocking { Files.isDirectory(path) }
        .refineOrDie { case e: IOException => e }

    override def listDirectory(path: Path): Stream[IOException, Path] =
      Stream.fromEffect(blocking.effectBlocking { Files.list(path).toScala(Iterable) }.refineOrDie { case e: IOException => e })
        .flatMap(Stream.fromIterable)


    override def zipEntries[R, E](errorHandler: IOException => E)(entries: ArStream[ZIO, R, E, ZipEntryInfo[ZIO, R, E]]): ArStream[ZIO, R, E, Byte] =
      ZipEntryStreamTransformation(errorHandler, blocking)(entries)

    override def openZipFile[E](errorHandler: IOException => E)(path: Path): Managed[E, ZipFileReader[E]] =
      ZManaged.fromAutoCloseable(
        blocking.effectBlocking { new ZipFile(path.toFile) }
          .refineOrDie { case e: IOException => errorHandler(e) }
      )
        .map { zipFile =>
          new ZipFileReader[E] {
            override def getEntryStream(name: String): ArStream[ZIO, Any, E, Byte] =
              new InputStreamStream(errorHandler, blocking)(
                Resource.fromZManaged(
                  ZManaged.fromAutoCloseable(
                    blocking.effectBlocking { zipFile.getInputStream(zipFile.getEntry(name)) }
                      .refineOrDie {
                        case ex: IOException => errorHandler(ex)
                      }
                  )
                )
              )
          }
        }

    override def protocolBufferSink[E, A <: GeneratedMessage with Message[A]](errorHandler: IOException => E)(companion: GeneratedMessageCompanion[A]): StreamTransformation[ZIO, Any, E, Byte, Unit, Nothing, A] =
      InputStreamReaderTransformation(blocking) { stream =>
        blocking.effectBlocking {
          companion.parseFrom(stream)
        }
          .refineOrDie {
            case ex: IOException => errorHandler(ex)
          }
      }


    override def protocolBufferStream[E](errorHandler: IOException => E)(message: GeneratedMessage): ArStream[ZIO, Any, E, Byte] =
      OutputStreamWriterStream(blocking) { stream =>
        blocking.effectBlocking {
          message.writeTo(stream)
        }
          .refineOrDie {
            case ex: IOException => errorHandler(ex)
          }
      }
  }

  override val blocking: Blocking.Service[Any] = otherEnv.blocking
  override val console: Console.Service[Any] = otherEnv.console
  override val system: System.Service[Any] = otherEnv.system
}
