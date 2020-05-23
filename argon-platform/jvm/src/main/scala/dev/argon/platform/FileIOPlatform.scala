package dev.argon.platform

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import java.util.zip.ZipFile

import dev.argon.io.{Path, ZipFileReader}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import zio.{Chunk, IO, Managed, UIO, ZIO, ZLayer, ZManaged, stream}
import zio.blocking.Blocking
import zio.console.Console
import zio.stream.{Stream, ZSink, ZStream}
import zio.system.System
import dev.argon.io.fileio.FileIO

import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import dev.argon.io.Path.PathExtensions

@SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
private[platform] object FileIOPlatform {

  val live: ZLayer[Blocking, Nothing, FileIO[FilePath]] = ZLayer.fromFunction { env =>
    val blocking = env.get[Blocking.Service]

    new FileIO.Service[FilePath] {

      override def getAbsolutePath(path: FilePath): IO[IOException, FilePath] =
        blocking.effectBlocking { path.javaPath.toAbsolutePath.toFilePath }
          .refineToOrDie[IOException]

      override def ensureParentDirectory(path: FilePath): IO[IOException, Unit] =
        blocking.effectBlocking { path.parent.foreach { dir => Files.createDirectories(dir.javaPath) } }
          .refineToOrDie[IOException]

      override def readAllText(path: FilePath): IO[IOException, String] =
        blocking.effectBlocking { Files.readString(path.javaPath, StandardCharsets.UTF_8) }
          .refineOrDie { case e: IOException => e }

      override def readText[E](errorHandler: IOException => E)(path: FilePath): Stream[E, Char] =
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


      override def writeToFile[R, E](errorHandler: IOException => E)(path: FilePath)(data: ZStream[R, E, Byte]): ZIO[R, E, Unit] =
        ZManaged.fromAutoCloseable(
          blocking.effectBlocking { Files.newOutputStream(path.javaPath, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING) }
        )
          .refineOrDie {
            case ex: IOException => errorHandler(ex)
          }
          .use { outputStream =>
            data.foreachChunk { chunk =>
              blocking.effectBlocking { outputStream.write(chunk.toArray) }
                .refineOrDie {
                  case ex: IOException => errorHandler(ex)
                }
            }
          }

      override def isDirectory(path: FilePath): IO[IOException, Boolean] =
        blocking.effectBlocking { Files.isDirectory(path.javaPath) }
          .refineOrDie { case e: IOException => e }

      override def listDirectory(path: FilePath): Stream[IOException, FilePath] =
        filePathIsJavaPath.flip.substitute(
          Stream.fromEffect(
            blocking.effectBlocking { Files.list(path.javaPath).toScala(Iterable) }
              .refineOrDie { case e: IOException => e }
          )
            .flatMap(Stream.fromIterable(_))
        )

      override def openZipFile[R, E](errorHandler: IOException => E)(path: FilePath): Managed[E, ZipFileReader[R, E]] =
        ZManaged.fromAutoCloseable(
          blocking.effectBlocking { new ZipFile(path.javaPath.toFile) }
            .refineOrDie { case e: IOException => errorHandler(e) }
        )
          .map { zipFile =>
            new ZipFileReader[R, E] {
              override def getEntryStream(name: String): ZStream[R, E, Byte] =
                  ZStream.managed(
                    ZManaged.fromAutoCloseable(
                      blocking.effectBlocking { zipFile.getInputStream(zipFile.getEntry(name)) }
                        .refineOrDie {
                          case ex: IOException => errorHandler(ex)
                        }
                    )
                  )
                  .flatMap(ZStream.fromInputStream(_).mapError(errorHandler).provide(env))
            }
          }

    }
  }

}
