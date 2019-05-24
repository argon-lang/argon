package dev.argon.compiler

import java.io
import java.io.File
import java.util.zip

import dev.argon.util.{FileOperations, FilenameManip}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import cats._
import scalaz.zio._
import scalaz.zio.interop._
import scalaz.zio.interop.catz._
import FileOperations.fileShow
import cats.data.NonEmptyList
import com.google.protobuf.InvalidProtocolBufferException
import dev.argon.util.stream.{ArStream, OutputStreamTransformation, OutputStreamWriterStream, Resource, StreamTransformation, ZipEntryInfo, ZipEntryStreamTransformation}
import scalaz.zio

trait IOCompilation extends CompilationExec[ZIO, UIO]

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


    override def flatMap[A, B](fa: IO[NonEmptyList[CompilationError], A])(f: A => IO[NonEmptyList[CompilationError], B]): IO[NonEmptyList[CompilationError], B] =
      fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => IO[NonEmptyList[CompilationError], Either[A, B]]): IO[NonEmptyList[CompilationError], B] =
      implicitly[Monad[IO[NonEmptyList[CompilationError], ?]]].tailRecM(a)(f)

    override def pure[A](x: A): IO[NonEmptyList[CompilationError], A] = IO.succeed(x)

    override def getResult[A](fa: IO[E, A]): UIO[(Vector[CompilationMessageNonFatal], Either[E, A])] = for {
      eitherRes <- fa.either
    } yield (Vector(), eitherRes)

  })

  trait IOResourceAccess extends ResourceAccess[ZIO, io.File] {
    override type InputStream = io.InputStream
    override type ZipReader = zip.ZipFile
  }

  implicit val fileSystemResourceAccess: IOResourceAccess =
    new IOResourceAccess {

      private def ioExceptionToError(ex: io.IOException): NonEmptyList[CompilationError] =
        NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))

      private def handleIOException[A](value: IO[io.IOException, Either[NonEmptyList[CompilationError], A]]): IO[NonEmptyList[CompilationError], A] =
        value.either.flatMap { either =>
          IO.fromEither(
            either.left.map(ioExceptionToError)
              .flatMap(identity)
          )
        }

      override def getExtension(id: io.File): UIO[String] = IO.effectTotal {
        FilenameManip.getExtension(id)
      }

      override def resourceSink(id: File): Resource[ZIO, Any, NonEmptyList[CompilationError], StreamTransformation[ZIO, Any, NonEmptyList[CompilationError], Byte, Unit, Nothing, Unit]] =
        new Resource[ZIO, Any, NonEmptyList[CompilationError], StreamTransformation[ZIO, Any, NonEmptyList[CompilationError], Byte, Unit, Nothing, Unit]] {
          override def use[R2 <: Any, E2 >: NonEmptyList[CompilationError], B](f: StreamTransformation[ZIO, Any, NonEmptyList[CompilationError], Byte, Unit, Nothing, Unit] => ZIO[R2, E2, B]): ZIO[R2, E2, B] =
            IO.effect { new io.FileOutputStream(id) }.refineOrDie {
              case ex: io.IOException => ioExceptionToError(ex)
            }.bracketAuto { outputStream =>
              f(OutputStreamTransformation(ioExceptionToError)(outputStream))
            }
        }

      override def zipFromEntries(entryStream: ArStream[ZIO, Any, NonEmptyList[CompilationError], ZipEntryInfo[ZIO, Any, NonEmptyList[CompilationError]]]): ArStream[ZIO, Any, NonEmptyList[CompilationError], Byte] =
        ZipEntryStreamTransformation(ioExceptionToError)(entryStream)

      override def getZipReader[A](id: io.File)(f: ZipReader => IO[NonEmptyList[CompilationError], A]): IO[NonEmptyList[CompilationError], A] =
        handleIOException(FileOperations.createZipFile(id)(zipFile => f(zipFile).either))

      override def getZipEntryInputStream[A](zip: ZipReader, name: String)(f: InputStream => IO[NonEmptyList[CompilationError], A]): IO[NonEmptyList[CompilationError], A] =
        handleIOException(FileOperations.getZipEntryStream(zip, name)(stream => f(stream).either))

      override def readProtocolBufferMessage[A <: GeneratedMessage with Message[A]](companion: GeneratedMessageCompanion[A])(stream: InputStream): IO[NonEmptyList[CompilationError], A] =
        IO.effect { companion.parseFrom(stream) }.refineOrDie {
          case ex: io.IOException => ioExceptionToError(ex)
        }

      override def protocolBufferStream(message: GeneratedMessage): ArStream[ZIO, Any, NonEmptyList[CompilationError], Byte] =
        OutputStreamWriterStream(stream => IO.effect { message.writeTo(stream) }.refineOrDie {
          case ex: io.IOException => ioExceptionToError(ex)
        })
    }

}
