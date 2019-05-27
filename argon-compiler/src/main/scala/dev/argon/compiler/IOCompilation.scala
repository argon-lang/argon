package dev.argon.compiler

import java.io
import java.io.File
import java.util.zip
import java.util.zip.ZipFile

import dev.argon.util.{FileOperations, FilenameManip}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import cats._
import scalaz.zio._
import scalaz.zio.interop._
import scalaz.zio.interop.catz._
import FileOperations.fileShow
import cats.data.NonEmptyList
import com.google.protobuf.InvalidProtocolBufferException
import dev.argon.util.stream.{ArStream, InputStreamReaderTransformation, InputStreamStream, OutputStreamTransformation, OutputStreamWriterStream, PureEffect, Resource, StreamTransformation, ZipEntryInfo, ZipEntryStreamTransformation}
import scalaz.zio
import scalaz.zio.blocking.Blocking

trait IOCompilation[R] extends CompilationExec[ZIO, R]

object IOCompilation {

  def compilationInstance[R]: UIO[IOCompilation[R]] = IO.succeed(new IOCompilation[R] {

    type E = ErrorType
    type F[+A] = ZIO[R, E, A]

    override def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[Nothing]): F[A] =
      IO.fail(errors)

    override def createCache[A]: F[F[A] => F[A]] =
      ZIO.environment[R].flatMap { env =>
        RefM.make(Option.empty[Promise[E, A]]).map { ref => createValue =>
          ref.modify {
            case value @ Some(promise) => IO.succeed((promise, value))
            case None =>
              for {
                promise <- Promise.make[E, A]
                _ <- promise.done(createValue.provide(env)).fork
              } yield (promise, Some(promise))
          }
            .flatMap[Any, E, A] { promise => promise.await }
        }
      }

    override def createMemo[A, B]: F[(A => F[B]) => A => F[B]] =
      ZIO.environment[R].flatMap { env =>
        RefM.make(Map[A, Promise[E, B]]()).map { ref => createValue => a =>
          ref.modify { map =>
            map.get(a) match {
              case Some(promise) => IO.succeed((promise, map))
              case None =>
                for {
                  promise <- Promise.make[E, B]
                  _ <- promise.done(createValue(a).provide(env)).fork
                } yield (promise, map + (a -> promise))
            }
          }
            .flatMap[Any, E, B] { promise => promise.await }
        }
      }


    override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
      fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
      implicitly[Monad[F]].tailRecM(a)(f)

    override def pure[A](x: A): F[A] = IO.succeed(x)

    override def fromPureEffect[A](pa: PureEffect[R, E, A]): ZIO[R, E, A] =
      ZIO.environment[R].flatMap { r => IO.fromEither(pa.run(r).value) }

    override def getResult[A](fa: F[A]): ZIO[R, Nothing, (Vector[CompilationMessageNonFatal], Either[E, A])] = for {
      eitherRes <- fa.either
    } yield (Vector(), eitherRes)

  })

  trait IOResourceAccess extends ResourceAccess[ZIO, Blocking, io.File] {
    override type ZipReader = zip.ZipFile
  }

  implicit val fileSystemResourceAccess: IOResourceAccess =
    new IOResourceAccess {

      private def ioExceptionToError(ex: io.IOException): NonEmptyList[CompilationError] =
        NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))

      override def getExtension(id: io.File): UIO[String] = IO.effectTotal {
        FilenameManip.getExtension(id)
      }

      override def resourceSink(id: File): Resource[ZIO, Blocking, NonEmptyList[CompilationError], StreamTransformation[ZIO, Blocking, NonEmptyList[CompilationError], Byte, Unit, Nothing, Unit]] =
        Resource.fromZManaged(ZManaged.make(
          ZIO.environment[Blocking].flatMap(_.blocking.effectBlocking { new io.FileOutputStream(id) }).refineOrDie {
            case ex: io.IOException => ioExceptionToError(ex)
          }
        )(
          _.closeIO()
        ))
          .map { outputStream => OutputStreamTransformation(ioExceptionToError)(outputStream) }

      override def zipFromEntries(entryStream: ArStream[ZIO, Blocking, NonEmptyList[CompilationError], ZipEntryInfo[ZIO, Blocking, NonEmptyList[CompilationError]]]): ArStream[ZIO, Blocking, NonEmptyList[CompilationError], Byte] =
        ZipEntryStreamTransformation(ioExceptionToError)(entryStream)

      override def getZipReader[A](id: io.File): Resource[ZIO, Blocking, NonEmptyList[CompilationError], ZipReader] =
        Resource.fromZManaged(
          ZManaged.make(
            ZIO.environment[Blocking].flatMap(_.blocking.effectBlocking { new zip.ZipFile(id) })
              .refineOrDie { case e: io.IOException => ioExceptionToError(e) }
          )(
            _.closeIO()
          )
        )

      override def zipEntryStream(zip: ZipFile, name: String): ArStream[ZIO, Blocking, NonEmptyList[CompilationError], Byte] =
        new InputStreamStream(ioExceptionToError)(
          Resource.fromZManaged(
            ZManaged.make(
              ZIO.environment[Blocking]
                .flatMap(_.blocking.effectBlocking {
                  zip.getInputStream(zip.getEntry(name))
                })
                .refineOrDie {
                  case ex: io.IOException => ioExceptionToError(ex)
                }

            )(_.closeIO())
          )
        )

      override def protocolBufferSink[A <: GeneratedMessage with Message[A]](companion: GeneratedMessageCompanion[A]): StreamTransformation[ZIO, Blocking, NonEmptyList[CompilationError], Byte, Unit, Nothing, A] =
        InputStreamReaderTransformation { stream =>
          ZIO.environment[Blocking]
            .flatMap(_.blocking.effectBlocking {
              companion.parseFrom(stream)
            })
            .refineOrDie {
              case ex: io.IOException => ioExceptionToError(ex)
            }
        }

      override def protocolBufferStream(message: GeneratedMessage): ArStream[ZIO, Blocking, NonEmptyList[CompilationError], Byte] =
        OutputStreamWriterStream { stream =>
          ZIO.environment[Blocking]
            .flatMap(_.blocking.effectBlocking {
              message.writeTo(stream)
            })
            .refineOrDie {
              case ex: io.IOException => ioExceptionToError(ex)
            }
        }
    }

}
