package dev.argon.compiler

import java.io.{File, InputStream, OutputStream, PrintWriter}
import java.util.Locale
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}

import dev.argon.util.{FileOperations, FilenameManip}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, Message}
import scalaz._
import scalaz.zio._
import scalaz.zio.interop.scalaz72._

trait IOCompilation extends CompilationExec[Task, Task]

object IOCompilation {

  final case class CompilationErrorException(errors: NonEmptyList[CompilationError]) extends Exception

  val compilationInstance: UIO[IOCompilation] = for {
    messageAccum <- Ref.make(Vector.empty[CompilationMessageNonFatal])
  } yield new IOCompilation {
    override def diagnostic[A](value: A, messages: Vector[CompilationMessageNonFatal]): Task[A] = for {
      _ <- messageAccum.update { acc => acc ++ messages }
    } yield value

    override def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[CompilationMessageNonFatal]): Task[A] =
      messageAccum.update { acc => acc ++ messages }.flatMap { _ => IO.fail(CompilationErrorException(errors)) }

    override def createCache[A]: Task[Task[A] => Task[A]] =
      RefM.make(Option.empty[Promise[Throwable, A]]).map { ref => createValue =>
        ref.modify {
          case value @ Some(promise) => IO.succeed((promise, value))
          case None =>
            for {
              promise <- Promise.make[Throwable, A]
              _ <- promise.done(createValue).fork
            } yield (promise, Some(promise))
        }
          .flatMap { promise => promise.await }
      }

    override def createMemo[A, B]: Task[(A => Task[B]) => A => Task[B]] =
      RefM.make(Map[A, Promise[Throwable, B]]()).map { ref => createValue => a =>
        ref.modify { map =>
          map.get(a) match {
            case Some(promise) => IO.succeed((promise, map))
            case None =>
              for {
                promise <- Promise.make[Throwable, B]
                _ <- promise.done(createValue(a)).fork
              } yield (promise, map + (a -> promise))
          }
        }
          .flatMap { promise => promise.await }
      }

    override def point[A](a: => A): Task[A] = IO(a)

    override def bind[A, B](fa: Task[A])(f: A => IO[Throwable, B]): IO[Throwable, B] =
      fa.flatMap(f)


    override def getResult[A](fa: Task[A]): IO[Throwable, (Vector[CompilationMessageNonFatal], Either[NonEmptyList[CompilationError], A])] =
      fa
        .flatMap { a =>
          messageAccum.get.map { messages =>
            (messages, Right(a))
          }
        }
        .catchSome {
          case CompilationErrorException(errors) =>
            messageAccum.get.map { messages =>
              (messages, Left(errors))
            }
        }
  }

  implicit val fileSystemResourceAccess: ResourceAccess[Task, File] =
    new ResourceAccess[Task, File] {

      override def getExtension(id: File): Task[String] = IO.effect {
        FilenameManip.getExtension(id)
      }

      override def createPrintWriter[A](id: File)(f: PrintWriter => A): Task[A] =
        FileOperations.filePrintWriter(id) { writer => IO.effect { f(writer) } }

      override def createOutputStream[A](id: File)(f: OutputStream => Task[A]): Task[A] =
        FileOperations.fileOutputStream(id)(f)

      override def createZipOutputStream[A](stream: OutputStream)(f: ZipOutputStream => Task[A]): Task[A] =
        FileOperations.zipOutputStream(stream)(f)

      override def createZipEntry[A](zip: ZipOutputStream, path: String)(f: ZipEntry => Task[A]): Task[A] =
        FileOperations.createZipEntry(zip, path)(f)


      override type ZipReader = ZipFile
      override def getZipFile[A](id: File)(f: ZipFile => Task[A]): Task[A] =
        FileOperations.createZipFile(id)(f)

      override def getZipEntryStream[A](zip: ZipFile, name: String)(f: InputStream => Task[A]): Task[A] =
        FileOperations.getZipEntryStream(zip, name)(f)

      override def readProtocolBufferMessage[A <: GeneratedMessage with Message[A]](companion: GeneratedMessageCompanion[A])(stream: InputStream): Task[A] = IO.effect {
        companion.parseFrom(stream)
      }

      override def writeProtocolBufferMessage(stream: OutputStream, message: GeneratedMessage): Task[Unit] = IO.effect {
        message.writeTo(stream)
      }
    }

}
