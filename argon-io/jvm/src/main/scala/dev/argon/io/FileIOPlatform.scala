package dev.argon.io

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import java.util.zip.ZipFile

import dev.argon.stream.builder.{Source, SourceIO, ZStreamSource}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import zio.{Chunk, IO, Managed, UIO, ZIO, ZLayer, ZManaged, stream}
import zio.blocking.Blocking
import zio.console.Console
import zio.stream.{Stream, ZStream}
import zio.system.System
import dev.argon.io.fileio.FileIO

import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
trait FileIOPlatform {

  val live: ZLayer[Blocking, Nothing, FileIO] = ZLayer.fromFunction { env =>
    val blocking = env.get[Blocking.Service]

    new FileIO.Service {

      override def getEnv(name: String): UIO[Option[String]] =
        IO.effectTotal { Option(java.lang.System.getenv(name)) }

      override def getAbsolutePath(path: Path): IO[IOException, Path] =
        blocking.effectBlocking { new Path(path.javaPath.toAbsolutePath) }
          .refineOrDie { case e: IOException => e }

      override def readAllText(path: Path): IO[IOException, String] =
        blocking.effectBlocking { Files.readString(path.javaPath, StandardCharsets.UTF_8) }
          .refineOrDie { case e: IOException => e }

      override def readText[E](errorHandler: IOException => E)(path: Path): Stream[E, Char] =
        stream.ZStream.managed(
          ZManaged.fromAutoCloseable(
            blocking.effectBlocking { Files.newBufferedReader(path.javaPath, StandardCharsets.UTF_8) }
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
          blocking.effectBlocking { Files.newOutputStream(path.javaPath, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING) }
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
        blocking.effectBlocking { Files.isDirectory(path.javaPath) }
          .refineOrDie { case e: IOException => e }

      override def listDirectory(path: Path): Stream[IOException, Path] =
        Stream.fromEffect(
          blocking.effectBlocking { Files.list(path.javaPath).toScala(Iterable) }
            .refineOrDie { case e: IOException => e }
        )
          .flatMap(Stream.fromIterable(_))
          .map(new Path(_))

      override def zipEntries[R, E](errorHandler: IOException => E)(entries: Source[ZIO[R, E, *], ZipEntryInfo[ZIO[R, E, *]], Unit]): Source[ZIO[R, E, *], Chunk[Byte], Unit] =
        ZStreamSource(
          ZipEntryStreamTransformation[R, E](errorHandler, env)(entries)
        )

      override def openZipFile[R, E](errorHandler: IOException => E)(path: Path): Managed[E, ZipFileReader[ZIO[R, E, *]]] =
        ZManaged.fromAutoCloseable(
          blocking.effectBlocking { new ZipFile(path.javaPath.toFile) }
            .refineOrDie { case e: IOException => errorHandler(e) }
        )
          .map { zipFile =>
            new ZipFileReader[ZIO[R, E, *]] {
              override def getEntryStream(name: String): Source[ZIO[R, E, *], Chunk[Byte], Unit] = {
                val stream =
                  ZStream.managed(
                    ZManaged.fromAutoCloseable(
                      blocking.effectBlocking { zipFile.getInputStream(zipFile.getEntry(name)) }
                        .refineOrDie {
                          case ex: IOException => errorHandler(ex)
                        }
                    )
                  )
                  .flatMap(ZStream.fromInputStream(_).chunks.mapError(errorHandler))

                ZStreamSource(stream)
              }
            }
          }

      override def deserializeProtocolBuffer[R, E, A <: GeneratedMessage](errorHandler: IOException => E)(companion: GeneratedMessageCompanion[A])(data: Source[ZIO[R, E, *], Chunk[Byte], Unit]): ZIO[R, E, A] =
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
  }

}
