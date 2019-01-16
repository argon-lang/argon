package com.mi3software.argon.compiler

import java.io.File
import java.util.Locale

import com.twitter.scrooge.{ThriftStruct, ThriftStructCodec}
import org.apache.commons.io.FilenameUtils
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.transport.TSimpleFileTransport
import scalaz.{NonEmptyList, \/}
import scalaz.zio._

trait IOCompilation extends CompilationExec[IO[Throwable, ?], IO[Throwable, ?]]

object IOCompilation {

  final case class CompilationErrorException(errors: NonEmptyList[CompilationError]) extends Exception

  val compilationInstance: IO[Nothing, IOCompilation] = for {
    messageAccum <- Ref(Vector.empty[CompilationMessageNonFatal])
  } yield new IOCompilation {
    override def diagnostic[A](value: A, messages: Vector[CompilationMessageNonFatal]): IO[Throwable, A] = for {
      _ <- messageAccum.update { acc => acc ++ messages }
    } yield value

    override def forErrors[A](errors: NonEmptyList[CompilationError], messages: Vector[CompilationMessageNonFatal]): IO[Throwable, A] =
      messageAccum.update { acc => acc ++ messages }.flatMap { _ => IO.fail(CompilationErrorException(errors)) }

    override def point[A](a: => A): IO[Throwable, A] = IO.point(a)

    override def bind[A, B](fa: IO[Throwable, A])(f: A => IO[Throwable, B]): IO[Throwable, B] =
      fa.flatMap(f)



    /*

    override def getMessages: IO[Nothing, Vector[CompilationMessageNonFatal]] = messageAccum.get

    override def getResult[A](fa: IO[Throwable, A]): IO[Throwable, NonEmptyList[CompilationError] \/ A] =
      */
    override def getResult[A](fa: IO[Throwable, A]): IO[Throwable, (Vector[CompilationMessageNonFatal], NonEmptyList[CompilationError] \/ A)] =
      fa
        .flatMap { a =>
          messageAccum.get.map { messages =>
            (messages, \/.right[NonEmptyList[CompilationError], A](a))
          }
        }
        .catchSome {
          case CompilationErrorException(errors) =>
            messageAccum.get.map { messages =>
              (messages, \/.left[NonEmptyList[CompilationError], A](errors))
            }
        }
  }

  implicit val fileSystemResourceAccess: ResourceAccess[IO[Throwable, ?], File] =
    new ResourceAccess[IO[Throwable, ?], File] {

      override def getExtension(id: File): IO[Throwable, String] = IO.syncThrowable {
        FilenameUtils.getExtension(id.getName).toLowerCase(Locale.ENGLISH)
      }

      override def loadThriftStruct[T <: ThriftStruct](id: File)(codec: ThriftStructCodec[T]): IO[Throwable, T] = IO.syncThrowable {
        val trans = new TSimpleFileTransport(id.getPath)
        val prot = new TBinaryProtocol(trans)
        codec.decode(prot)
      }

    }

}
