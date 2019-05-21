package dev.argon.compiler

import java.io
import java.util.zip

import dev.argon.util.{FileOperations, FilenameManip}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import scalaz._
import scalaz.zio._
import scalaz.zio.interop.scalaz72._
import FileOperations.fileShow
import com.google.protobuf.InvalidProtocolBufferException

trait IOCompilation extends CompilationExec[IO, UIO]

object IOCompilation {

  val compilationInstance: UIO[IOCompilation] = IO.succeed(new IOCompilation {

    type E = ErrorType

    override def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[Nothing]): IO[E, A] =
      IO.fail(errors)

    override def createCache[A]: IO[E, IO[E, A] => IO[E, A]] =
      RefM.make(Option.empty[Promise[E, A]]).map { ref => createValue =>
        ref.modify {
          case value @ Some(promise) => IO.succeed((promise, value))
          case None =>
            for {
              promise <- Promise.make[E, A]
              _ <- promise.done(createValue).fork
            } yield (promise, Some(promise))
        }
          .flatMap { promise => promise.await }
      }

    override def createMemo[A, B]: IO[E, (A => IO[E, B]) => A => IO[E, B]] =
      RefM.make(Map[A, Promise[E, B]]()).map { ref => createValue => a =>
        ref.modify { map =>
          map.get(a) match {
            case Some(promise) => IO.succeed((promise, map))
            case None =>
              for {
                promise <- Promise.make[E, B]
                _ <- promise.done(createValue(a)).fork
              } yield (promise, map + (a -> promise))
          }
        }
          .flatMap { promise => promise.await }
      }


    override def point[A](a: => A): UIO[A] = IO.succeed(a)

    override def bind[A, B](fa: IO[E, A])(f: A => IO[E, B]): IO[E, B] =
      fa.flatMap(f)

    override def getResult[A](fa: IO[E, A]): UIO[(Vector[CompilationMessageNonFatal], Either[E, A])] = for {
      eitherRes <- fa.either
    } yield (Vector(), eitherRes)

  })

  trait IOResourceAccess extends ResourceAccess[IO[NonEmptyList[CompilationError], ?], io.File] {
    override type PrintWriter = (io.PrintWriter, io.File)
    override type OutputStream = (io.OutputStream, io.File)
    override type InputStream = (io.InputStream, io.File)
    override type ZipWriter = (zip.ZipOutputStream, io.File)
    override type ZipReader = (zip.ZipFile, io.File)
  }

  implicit val fileSystemResourceAccess: IOResourceAccess =
    new IOResourceAccess {

      private def handleIOException[A](file: io.File)(value: IO[io.IOException, Either[NonEmptyList[CompilationError], A]]): IO[NonEmptyList[CompilationError], A] =
        value.either.flatMap { either =>
          IO.fromEither(
            either.left.map { ex => NonEmptyList[CompilationError](CompilationError.ResourceIOError(CompilationMessageSource.ResourceIdentifier(file), ex)) }
              .flatMap(identity)
          )
        }

      override def getExtension(id: io.File): IO[NonEmptyList[CompilationError], String] = IO.effectTotal {
        FilenameManip.getExtension(id)
      }

      override def createPrintWriter[A](id: io.File)(f: PrintWriter => IO[NonEmptyList[CompilationError], A]): IO[NonEmptyList[CompilationError], A] =
        handleIOException(id)(FileOperations.filePrintWriter(id)(writer => f((writer, id)).either))

      override def createOutputStream[A](id: io.File)(f: OutputStream => IO[NonEmptyList[CompilationError], A]): IO[NonEmptyList[CompilationError], A] =
        handleIOException(id)(FileOperations.fileOutputStream(id)(stream => f((stream, id)).either))

      override def writeText(writer: PrintWriter, text: String): IO[NonEmptyList[CompilationError], Unit] =
        IO.effect { writer._1.print(text) }.refineOrDie {
          case ex: io.IOException => NonEmptyList[CompilationError](CompilationError.ResourceIOError(CompilationMessageSource.ResourceIdentifier(writer._2), ex))
        }

      override def createZipWriter[A](stream: OutputStream)(f: ZipWriter => IO[NonEmptyList[CompilationError], A]): IO[NonEmptyList[CompilationError], A] =
        handleIOException(stream._2)(FileOperations.zipOutputStream(stream._1)(zipStream => f((zipStream, stream._2)).either))

      override def writeZipEntry[A](zip: ZipWriter, path: String)(f: OutputStream => IO[NonEmptyList[CompilationError], A]): IO[NonEmptyList[CompilationError], A] =
        handleIOException(zip._2)(FileOperations.createZipEntry(zip._1, path)(_ => f(zip).either))

      override def getZipReader[A](id: io.File)(f: ZipReader => IO[NonEmptyList[CompilationError], A]): IO[NonEmptyList[CompilationError], A] =
        handleIOException(id)(FileOperations.createZipFile(id)(zipFile => f((zipFile, id)).either))

      override def getZipEntryInputStream[A](zip: ZipReader, name: String)(f: InputStream => IO[NonEmptyList[CompilationError], A]): IO[NonEmptyList[CompilationError], A] =
        handleIOException(zip._2)(FileOperations.getZipEntryStream(zip._1, name)(stream => f((stream, zip._2)).either))

      override def readProtocolBufferMessage[A <: GeneratedMessage with Message[A]](companion: GeneratedMessageCompanion[A])(stream: InputStream): IO[NonEmptyList[CompilationError], A] =
        IO.effect { companion.parseFrom(stream._1) }.refineOrDie {
          case ex: io.IOException => NonEmptyList[CompilationError](CompilationError.ResourceIOError(CompilationMessageSource.ResourceIdentifier(stream._2), ex))
          case _: InvalidProtocolBufferException => NonEmptyList[CompilationError](CompilationError.InvalidProtocolBufferMessage(CompilationMessageSource.ResourceIdentifier(stream._2)))
        }

      override def writeProtocolBufferMessage(stream: OutputStream, message: GeneratedMessage): IO[NonEmptyList[CompilationError], Unit] =
        IO.effect { message.writeTo(stream._1) }.refineOrDie {
          case ex: io.IOException => NonEmptyList[CompilationError](CompilationError.ResourceIOError(CompilationMessageSource.ResourceIdentifier(stream._2), ex))
          case _: InvalidProtocolBufferException => NonEmptyList[CompilationError](CompilationError.InvalidProtocolBufferMessage(CompilationMessageSource.ResourceIdentifier(stream._2)))
        }
    }

}
