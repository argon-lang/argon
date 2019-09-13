package dev.argon.compiler

import java.io
import dev.argon.io.Path
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
import dev.argon.stream.builder.Source

trait IOCompilation[R] extends Compilation[ZIO[R, NonEmptyList[CompilationError], *]] {
  def getResult[A](fa: ZIO[R, NonEmptyList[CompilationError], A]): ZIO[R, Nothing, (Vector[CompilationMessageNonFatal], Either[NonEmptyList[CompilationError], A])]
}

object IOCompilation {

  def compilationInstance[R]: UIO[IOCompilation[R]] = IO.succeed(new IOCompilation[R] {

    type E = NonEmptyList[CompilationError]
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
      zio.interop.catz.monadErrorInstance.tailRecM(a)(f)

    override def pure[A](x: A): F[A] = IO.succeed(x)

    override def getResult[A](fa: F[A]): ZIO[R, Nothing, (Vector[CompilationMessageNonFatal], Either[E, A])] = for {
      eitherRes <- fa.either
    } yield (Vector(), eitherRes)

  })

  type IOContext[R] = Backend.ContextWithComp[ZIO[R, NonEmptyList[CompilationError], +*], Path]

  trait IOResourceAccess[R, TContext <: IOContext[R] with Singleton] extends ResourceAccess[TContext] {
    override type ZipReader = ZipFileReader[ZIO[R, NonEmptyList[CompilationError], ?]]
  }

  def fileSystemResourceAccessFactory[R](fileIO: FileIO.Service): ResourceAccessFactory[IOContext[R]] = new ResourceAccessFactory[IOContext[R]] {
    override def create(context2: IOContext[R]): ResourceAccess[context2.type] =
      new IOResourceAccess[R, context2.type] {

        override val context: context2.type = context2

        import context.Comp

        private def ioExceptionToError(ex: io.IOException): NonEmptyList[CompilationError] =
          NonEmptyList.of(CompilationError.ResourceIOError(CompilationMessageSource.ThrownException(ex)))

        override def getExtension(id: Path): UIO[String] =
          IO.succeed(id.extension)

        override def writeToResource[X](id: Path)(data: Source[Comp, Chunk[Byte], X]): Comp[X] =
          fileIO.writeToFile(ioExceptionToError)(id)(data)

        override def zipFromEntries(entries: Source[Comp, ZipEntryInfo[Comp], Unit]): Source[Comp, Chunk[Byte], Unit] =
          fileIO.zipEntries(ioExceptionToError)(entries)

        override def getZipReader[A](id: Path): Resource[ZIO[R, NonEmptyList[CompilationError], *], ZipReader] =
          Resource.fromZManaged(
            fileIO.openZipFile(ioExceptionToError)(id)
          )


        override def zipEntryStream(zip: ZipFileReader[ZIO[R, NonEmptyList[CompilationError], *]], name: String): Source[Comp, Chunk[Byte], Unit] =
          zip.getEntryStream(name)


        override def deserializeProtocolBuffer[L[_, _], A <: GeneratedMessage with Message[A]](companion: GeneratedMessageCompanion[A])(data: Source[Comp, Chunk[Byte], Unit]): Comp[A] =
          fileIO.deserializeProtocolBuffer(ioExceptionToError)(companion)(data)

        override def serializeProtocolBuffer(message: GeneratedMessage): Source[Comp, Chunk[Byte], Unit] =
         fileIO.serializeProtocolBuffer(ioExceptionToError)(message)

      }
  }


}
