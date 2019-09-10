package dev.argon.io

import java.io.{FileReader, IOException}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.zip.ZipFile

import dev.argon.stream._
import dev.argon.stream.builder.{Source, SourceIO, ZStreamSource}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import zio._
import zio.stream._
import zio.blocking.Blocking
import zio.console.Console
import zio.system.System
import zio.interop.catz._

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

    override def writeToFile[R, E, X](errorHandler: IOException => E)(path: Path)(data: Source[ZIO[R, E, *], Chunk[Byte], X]): ZIO[R, E, X] =
      ZManaged.fromAutoCloseable(
        blocking.effectBlocking { Files.newOutputStream(path, StandardOpenOption.CREATE) }
      )
        .refineOrDie {
          case ex: IOException => errorHandler(ex)
        }
        .use { outputStream =>
          data.foreach { chunk =>
            blocking.effectBlocking { outputStream.write(chunk.toArray) }
              .refineOrDie {
                case ex: IOException => errorHandler(ex)
              }
          }
        }

    override def isDirectory(path: Path): IO[IOException, Boolean] =
      blocking.effectBlocking { Files.isDirectory(path) }
        .refineOrDie { case e: IOException => e }

    override def listDirectory(path: Path): Stream[IOException, Path] =
      Stream.fromEffect(
        blocking.effectBlocking { Files.list(path).toScala(Iterable) }
          .refineOrDie { case e: IOException => e }
      )
        .flatMap(Stream.fromIterable)

    override def zipEntries[R, E](errorHandler: IOException => E)(entries: Source[ZIO[R, E, *], ZipEntryInfo[ZIO[R, E, *]], Unit]): Source[ZIO[R, E, *], Chunk[Byte], Unit] =
      ZStreamSource(
        ZipEntryStreamTransformation[R, E](errorHandler, blocking)(entries)
      )

    override def openZipFile[R, E](errorHandler: IOException => E)(path: Path): Managed[E, ZipFileReader[ZIO[R, E, *]]] =
      ZManaged.fromAutoCloseable(
        blocking.effectBlocking { new ZipFile(path.toFile) }
          .refineOrDie { case e: IOException => errorHandler(e) }
      )
        .map { zipFile =>
          new ZipFileReader[ZIO[R, E, ?]] {
            override def getEntryStream(name: String): Source[ZIO[R, E, ?], Chunk[Byte], Unit] = {
              val stream = InputStreamStream(errorHandler)(
                ZManaged.fromAutoCloseable(
                  blocking.effectBlocking { zipFile.getInputStream(zipFile.getEntry(name)) }
                    .refineOrDie {
                      case ex: IOException => errorHandler(ex)
                    }
                )
              ).provide(otherEnv)

              ZStreamSource(stream)
            }
          }
        }

    override def deserializeProtocolBuffer[R, E, A <: GeneratedMessage with Message[A]](errorHandler: IOException => E)(companion: GeneratedMessageCompanion[A])(data: Source[ZIO[R, E, *], Chunk[Byte], Unit]): ZIO[R, E, A] =
      InputStreamReaderTransformation(SourceIO.fromSource(data).toZStream) { stream =>
        blocking.effectBlocking {
          companion.parseFrom(stream)
        }
          .refineOrDie {
            case ex: IOException => errorHandler(ex)
          }
      }

    override def serializeProtocolBuffer[R, E](errorHandler: IOException => E)(message: GeneratedMessage): Source[ZIO[R, E, *], Chunk[Byte], Unit] =
      ZStreamSource(
        OutputStreamWriterStream { stream =>
          blocking.effectBlocking {
            message.writeTo(stream)
          }
            .refineOrDie {
              case ex: IOException => errorHandler(ex)
            }
        }
      )

  }

  override val blocking: Blocking.Service[Any] = otherEnv.blocking
  override val console: Console.Service[Any] = otherEnv.console
  override val system: System.Service[Any] = otherEnv.system
}
