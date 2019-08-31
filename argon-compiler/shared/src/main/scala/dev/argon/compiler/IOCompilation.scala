package dev.argon.compiler

import java.io
import java.nio.file.Path
import java.util.zip
import java.util.zip.ZipFile

import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import cats._
import zio._
import zio.interop._
import zio.interop.catz._
import cats.data.{NonEmptyList, NonEmptyVector}
import com.google.protobuf.InvalidProtocolBufferException
import dev.argon.compiler.backend.Backend
import dev.argon.compiler.core._
import dev.argon.io.{FileIO, FilenameManip, ZipEntryInfo, ZipFileReader}
import dev.argon.stream._

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
                _ <- promise.complete(createValue.provide(env)).fork
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
                  _ <- promise.complete(createValue(a).provide(env)).fork
                } yield (promise, map + (a -> promise))
            }
          }
            .flatMap[Any, E, B] { promise => promise.await }
        }
      }

    override def attempt[A](action: ZIO[R, NonEmptyList[CompilationError], A]): ZIO[R, NonEmptyList[CompilationError], Either[NonEmptyList[CompilationError], ZIO[R, NonEmptyList[CompilationError], A]]] =
      action.map(IO.succeed).either

    override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
      fa.flatMap(f)

    override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
      ZIO.effectSuspendTotal(f(a)).flatMap {
        case Left(l)  => tailRecM(l)(f)
        case Right(r) => ZIO.succeed(r)
      }

    override def pure[A](x: A): F[A] = IO.succeed(x)

    override def fromPureEffect[A](pa: PureEffect[R, E, A]): ZIO[R, E, A] =
      ZIO.environment[R].flatMap { r => IO.fromEither(pa.run(r).value) }

    override def getResult[A](fa: F[A]): ZIO[R, Nothing, (Vector[CompilationMessageNonFatal], Either[E, A])] = for {
      eitherRes <- fa.either
    } yield (Vector(), eitherRes)

  })

  type IOContext[R] = Backend.ContextWithComp[ZIO, R, Path]

  trait IOResourceAccess[R, TContext <: IOContext[R] with Singleton] extends ResourceAccess[TContext] {
    override type ZipReader = ZipFileReader[NonEmptyList[CompilationError]]
  }

  def fileSystemResourceAccessFactory[R](fileIO: FileIO.Service): ResourceAccessFactory[IOContext[R]] = new ResourceAccessFactory[IOContext[R]] {
    override def create(context2: IOContext[R]): ResourceAccess[context2.type] =
      new IOResourceAccess[R, context2.type] {

        override val context: context2.type = context2

        private def ioExceptionToError(ex: io.IOException): NonEmptyList[CompilationError] =
          NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))

        override def getExtension(id: Path): UIO[String] =
          IO.succeed(FilenameManip.getExtension(id))

        override def resourceSink(id: Path): Resource[ZIO, R, NonEmptyList[CompilationError], StreamTransformation[ZIO, R, NonEmptyList[CompilationError], Byte, Unit, Nothing, Unit]] =
          Resource.fromZManaged(
            fileIO.fileOutputTransformation(ioExceptionToError)(id)
          )

        override def zipFromEntries(entryStream: ArStream[ZIO, R, NonEmptyList[CompilationError], ZipEntryInfo[ZIO, R, NonEmptyList[CompilationError]]]): ArStream[ZIO, R, NonEmptyList[CompilationError], Byte] =
          fileIO.zipEntries(ioExceptionToError)(entryStream)

        override def getZipReader[A](id: Path): Resource[ZIO, R, NonEmptyList[CompilationError], ZipReader] =
          Resource.fromZManaged(
            fileIO.openZipFile(ioExceptionToError)(id)
          )

        override def zipEntryStream(zip: ZipReader, name: String): ArStream[ZIO, R, NonEmptyList[CompilationError], Byte] =
          zip.getEntryStream(name)

        override def protocolBufferSink[A <: GeneratedMessage with Message[A]](companion: GeneratedMessageCompanion[A]): StreamTransformation[ZIO, R, NonEmptyList[CompilationError], Byte, Unit, Nothing, A] =
          fileIO.protocolBufferSink(ioExceptionToError)(companion)


        override def protocolBufferStream(message: GeneratedMessage): ArStream[ZIO, R, NonEmptyList[CompilationError], Byte] =
          fileIO.protocolBufferStream(ioExceptionToError)(message)

      }
  }


}
