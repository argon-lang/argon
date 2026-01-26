package dev.argon.backend.scalaApi

import dev.argon.backend.api as javaApi
import dev.argon.backend.scalaApi.BinaryResource.WrappingIOErrorType
import dev.argon.nobleidl.runtime as nidlJava
import dev.argon.nobleidl.runtime.util.InputStreamWithError
import dev.argon.util.async.{ErrorWrapper, JavaExecuteIO}
import nobleidl.core.{ErrorType, JavaAdapter}
import dev.argon.io
import zio.*
import zio.stream.*

import java.io.{IOException, InterruptedIOException}
import java.util as ju
import scala.jdk.OptionConverters.*
import scala.reflect.TypeTest
import scala.compiletime.asMatchable

trait BinaryResource[E] {
  val fileName: Option[String]
  def asInputStream: IO[E, InputStreamWithError[WrappingIOException[E]]]
  def asBytes: Stream[E, Byte]

  protected[backend] def errorType: ErrorType[E]
  private given ErrorType[E] = errorType

  final def toIOResource: io.BinaryResource[E] =
    new io.BinaryResource[E] {
      override def fileName: Option[String] =
        BinaryResource.this.fileName

      override def asBytes: Stream[E, Byte] =
        BinaryResource.this.asBytes

      override def asInputStream[E1 >: E](using ew: ErrorWrapper[E1] { type EX <: IOException }): ZIO[Scope, ew.EX, InputStreamWithError[ew.EX]] =
        ZIO.fromAutoCloseable(ErrorWrapper.wrapEffect(BinaryResource.this.asInputStream)(using ew))
          .map { is =>
            import ew.given
            is.convertError[E, ew.EX, ew.EX](
              WrappingIOErrorType[E],
              e => ew.wrap(Cause.fail(e)),
              BinaryResource.ioErrorTypeFromWrapper[E1],
            )
          }
    }
}

object BinaryResource {

  def javaAdapter[SE, JE](eAdapter: JavaAdapter[SE, JE])(using Runtime[Any], ErrorType[SE], ErrorType[JE]): JavaAdapter[BinaryResource[SE], javaApi.BinaryResource[JE]] =
    new JavaAdapter[BinaryResource[SE], javaApi.BinaryResource[JE]] {
      override def toJava(s: BinaryResource[SE]): javaApi.BinaryResource[JE] =
        new javaApi.BinaryResource[JE] {
          override def fileName(): ju.Optional[String] =
            s.fileName.toJava

          override def asInputStream[EE <: IOException](errorType: nidlJava.util.IOErrorType[JE, EE]): InputStreamWithError[EE] =
            JavaExecuteIO.runInterruptableRaw(s.asInputStream.mapError(se => errorType.toThrowable(eAdapter.toJava(se))))
              .convertError(
                WrappingIOErrorType[SE](),
                eAdapter.toJava,
                errorType,
              )

        }



      override def fromJava(j: javaApi.BinaryResource[JE]): BinaryResource[SE] =
        new BinaryResource[SE] {
          override val fileName: Option[String] = j.fileName().toScala

          override protected[backend] def errorType: ErrorType[SE] = summon[ErrorType[SE]]

          override def asInputStream: IO[SE, InputStreamWithError[WrappingIOException[SE]]] =
            val errorTypeJ = WrappingIOErrorType[JE]()
            JavaExecuteIO.runJavaRaw {
              val errorTypeS = WrappingIOErrorType[SE]()
              j.asInputStream(errorTypeJ)
                .convertError(
                  errorTypeJ,
                  eAdapter.fromJava,
                  errorTypeS,
                )
            }
              .catchAll {
                case ex @ WrappingIOException(e) =>
                  summon[ErrorType[JE]].checkObject(e) match {
                    case Some(e) => ZIO.fail(eAdapter.fromJava(e))
                    case None => ZIO.die(ex)
                  }

                case ex => ZIO.die(ex)
              }

          override def asBytes: Stream[SE, Byte] =
            ZStream.unwrapScoped(
              asInputStream.map { is =>
                ZStream.fromInputStream(is)
                  .catchAll {
                    case ex: InterruptedIOException =>
                      ex.printStackTrace()
                      ZStream.fromZIO(ZIO.interrupt)
                    case ex @ WrappingIOException(e) =>
                      summon[ErrorType[JE]].checkObject(e) match {
                        case Some(e) => ZStream.fail(eAdapter.fromJava(e))
                        case None => ZStream.die(ex)
                      }

                    case ex => ZStream.die(ex)
                  }
              }
            )
        }



    }


  private[backend] class WrappingIOErrorType[E: ErrorType] extends nidlJava.util.IOErrorType[E, WrappingIOException[E]] {
    override def tryFromObject(o: Any): E | Null =
      summon[ErrorType[E]].checkObject(o).orNull

    override def tryFromThrowable(ex: Throwable): E | Null =
      ex match {
        case WrappingIOException(o) => tryFromObject(o)
        case _ => null
      }

    override def toThrowable(t: E): WrappingIOException[E] =
      WrappingIOException(t)
  }

  private[backend] def ioErrorTypeFromWrapper[E](using ew: ErrorWrapper[E] { type EX <: IOException }): nidlJava.util.IOErrorType[ew.EX, ew.EX] =
    new nidlJava.util.IOErrorType[ew.EX, ew.EX] {
      override def tryFromObject(o: Any): ew.EX | Null =
        o.asMatchable match {
          case ex: ew.EX => ex
          case _ => null
        }

      override def tryFromThrowable(throwable: Throwable): ew.EX | Null =
        throwable match {
          case ex: ew.EX => ex
          case _ => null
        }

      override def toThrowable(t: ew.EX): ew.EX = t
    }

  private trait WrappedJavaBinaryResource[E] {
    val javaBinaryResource: javaApi.BinaryResource[E]
  }

  private trait WrappedScalaBinaryResource[E] {
    val scalaBinaryResource: BinaryResource[E]
  }

}

