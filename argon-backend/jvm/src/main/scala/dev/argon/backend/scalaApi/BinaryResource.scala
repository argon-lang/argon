package dev.argon.backend.scalaApi

import dev.argon.backend.api as javaApi
import dev.argon.nobleidl.runtime as nidlJava
import dev.argon.nobleidl.runtime.util.InputStreamWithError
import dev.argon.util.async.{ErrorWrapper, JavaExecuteIO}
import nobleidl.core.{ErrorType, JavaAdapter}
import zio.*
import zio.stream.*

import java.io.{IOException, InterruptedIOException}
import java.util as ju
import scala.jdk.OptionConverters.*
import scala.reflect.TypeTest

trait BinaryResource[E] {
  val fileName: Option[String]
  def asInputStream: IO[E, InputStreamWithError[WrappingIOException[E]]]
  def asBytes: Stream[E, Byte]
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
                    case ex: InterruptedIOException => ZStream.fromZIO(ZIO.interrupt)
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


  private class WrappingIOErrorType[E: ErrorType] extends nidlJava.util.IOErrorType[E, WrappingIOException[E]] {
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

  private trait WrappedJavaBinaryResource[E] {
    val javaBinaryResource: javaApi.BinaryResource[E]
  }

  private trait WrappedScalaBinaryResource[E] {
    val scalaBinaryResource: BinaryResource[E]
  }

}

