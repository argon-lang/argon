package dev.argon.platform

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardOpenOption}
import java.util.jar.{JarFile, Manifest}
import java.util.zip.ZipFile

import dev.argon.io.{JarFileReader, Path, ZipFileReader}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import zio._
import zio.blocking.Blocking
import zio.console.Console
import zio.stream._
import zio.system.System
import dev.argon.io.fileio.FileIO

import scala.jdk.CollectionConverters._
import scala.jdk.StreamConverters._
import dev.argon.io.Path.PathExtensions

import java.lang.Runtime.{Version => JDKVersion}

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


      override def readFile[E](errorHandler: Throwable => Cause[E])(path: FilePath): Stream[E, Byte] =
        ZStream.fromFile(path.javaPath)
          .catchAll { ex => Stream.halt(errorHandler(ex)) }
          .provide(env)

      override def readAllText(path: FilePath): IO[IOException, String] =
        blocking.effectBlocking { Files.readString(path.javaPath, StandardCharsets.UTF_8) }
          .refineOrDie { case e: IOException => e }

      override def readText[E](errorHandler: Throwable => Cause[E])(path: FilePath): Stream[E, Char] =
        ZStream.fromFile(path.javaPath)
          .transduce(ZTransducer.utf8Decode)
          .flatMap { s => ZStream.fromChunk(Chunk.fromArray(s.toCharArray)) }
          .catchAll { ex => Stream.halt(errorHandler(ex)) }
          .provide(env)


      override def writeToFile[R, E](errorHandler: Throwable => Cause[E])(path: FilePath)(data: ZStream[R, E, Byte]): ZIO[R, E, Unit] = {
        val sink = ZSink.fromFile(path.javaPath)
          .foldM[Blocking, E, Byte, Byte, Unit](
            failure = { ex => ZSink.halt(errorHandler(ex)) },
            success = _ => ZSink.succeed(())
          )

        data.run(
          ZSink(sink.push.map { push => (opt: Option[Chunk[Byte]]) => push(opt).provide(env) }.provide(env))
        )
      }

      override def isDirectory(path: FilePath): IO[IOException, Boolean] =
        blocking.effectBlocking { Files.isDirectory(path.javaPath) }
          .refineOrDie { case e: IOException => e }

      override def listDirectory(path: FilePath): Stream[IOException, FilePath] =
        filePathIsJavaPath.flip.substitute(
          Stream.fromIterableM(
            blocking.effectBlocking { Files.list(path.javaPath).toScala(Iterable) }
              .refineOrDie { case e: IOException => e }
          )
        )

      override def openZipFile[R, E](errorHandler: Throwable => Cause[E])(path: FilePath): Managed[E, ZipFileReader[R, E]] =
        ZManaged.fromAutoCloseable(
          blocking.effectBlocking { new ZipFile(path.javaPath.toFile) }
            .catchAll { ex => IO.halt(errorHandler(ex)) }
        )
          .map { zipFile =>
            new ZipFileReader[R, E] {
              override def getEntryStream(name: String): ZIO[R, E, Option[ZStream[R, E, Byte]]] =
                blocking.effectBlocking { Option(zipFile.getEntry(name)) }
                  .catchAll { ex => IO.halt(errorHandler(ex)) }
                  .map { _.map { entry =>
                    ZStream.unwrapManaged(
                      ZManaged.fromAutoCloseable(
                        blocking.effectBlocking { zipFile.getInputStream(entry) }
                          .catchAll { ex => IO.halt(errorHandler(ex)) }
                      )
                        .map(ZStream.fromInputStream(_).catchAll { ex => Stream.halt(errorHandler(ex)) }.provide(env))
                    )
                  } }
            }
          }

      override def openJarFile[R, E](errorHandler: Throwable => Cause[E])(path: FilePath, jdkVersion: JDKVersion): Managed[E, JarFileReader[R,E]] =
        ZManaged.fromAutoCloseable(
          blocking.effectBlocking { new JarFile(path.javaPath.toFile, false, ZipFile.OPEN_READ, jdkVersion) }
            .catchAll { ex => IO.halt(errorHandler(ex)) }
        )
          .map { jarFile =>
            new JarFileReader[R, E] {


              override def jarName: ZIO[R, E, String] =
                IO.succeed(path.fileNameWithoutExtension)

              override def manifest: ZIO[R, E, Option[Manifest]] =
                blocking.effectBlocking { Option(jarFile.getManifest) }
                  .catchAll { ex => IO.halt(errorHandler(ex)) }



              override def getEntryStream(name: String): ZIO[R, E, Option[ZStream[R, E, Byte]]] =
                blocking.effectBlocking { Option(jarFile.getJarEntry(name)) }
                  .catchAll { ex => IO.halt(errorHandler(ex)) }
                  .map { _.map { entry =>
                    ZStream.unwrapManaged(
                      ZManaged.fromAutoCloseable(
                        blocking.effectBlocking { jarFile.getInputStream(entry) }
                          .catchAll { ex => IO.halt(errorHandler(ex)) }
                      )
                        .map(ZStream.fromInputStream(_).catchAll { ex => Stream.halt(errorHandler(ex)) }.provide(env))
                    )
                  } }
            }
          }


    }
  }

}
