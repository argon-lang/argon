package com.mi3software.argon.compiler

import java.io.{File, OutputStream, PrintWriter}
import java.util.Locale
import java.util.zip.{ZipEntry, ZipOutputStream}

import com.mi3software.argon.util.FileOperations
import com.twitter.scrooge.{ThriftStruct, ThriftStructCodec}
import org.apache.commons.io.FilenameUtils
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.transport.TSimpleFileTransport
import scalapb.GeneratedMessage
import scalaz._
import scalaz.zio._
import scalaz.zio.interop.scalaz72._

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

    override def createCache[A]: IO[Throwable, IO[Throwable, A] => IO[Throwable, A]] =
      RefM(Option.empty[Promise[Throwable, A]]).map { ref => createValue =>
        ref.modify {
          case value @ Some(promise) => IO.now((promise, value))
          case None =>
            for {
              promise <- Promise.make[Throwable, A]
              _ <- promise.done(createValue).fork
            } yield (promise, Some(promise))
        }
          .flatMap { promise => promise.get }
      }

    override def createMemo[A, B]: IO[Throwable, (A => IO[Throwable, B]) => A => IO[Throwable, B]] =
      RefM(Map[A, Promise[Throwable, B]]()).map { ref => createValue => a =>
        ref.modify { map =>
          map.get(a) match {
            case Some(promise) => IO.now((promise, map))
            case None =>
              for {
                promise <- Promise.make[Throwable, B]
                _ <- promise.done(createValue(a)).fork
              } yield (promise, map + (a -> promise))
          }
        }
          .flatMap { promise => promise.get }
      }

    override def point[A](a: => A): IO[Throwable, A] = IO.point(a)

    override def bind[A, B](fa: IO[Throwable, A])(f: A => IO[Throwable, B]): IO[Throwable, B] =
      fa.flatMap(f)


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

      override def createPrintWriter[A](id: File)(f: PrintWriter => A): IO[Throwable, A] =
        FileOperations.filePrintWriter(id) { writer => IO.syncThrowable { f(writer) } }

      override def createOutputStream[A](id: File)(f: OutputStream => IO[Throwable, A]): IO[Throwable, A] =
        FileOperations.fileOutputStream(id)(f)

      override def createZipOutputStream[A](stream: OutputStream)(f: ZipOutputStream => IO[Throwable, A]): IO[Throwable, A] =
        FileOperations.zipOutputStream(stream)(f)

      override def createZipEntry[A](zip: ZipOutputStream, path: String)(f: ZipEntry => IO[Throwable, A]): IO[Throwable, A] = ???

      override def writeProtocolBufferMessage(stream: OutputStream, message: GeneratedMessage): IO[Throwable, Unit] = ???
    }

}
