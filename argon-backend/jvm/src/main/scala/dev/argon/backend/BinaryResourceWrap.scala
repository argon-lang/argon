package dev.argon.backend

import dev.argon.backend.scalaApi.WrappingIOException
import dev.argon.io.BinaryResource
import dev.argon.nobleidl.runtime.util.{InputStreamWithError, RefiningInputStreamWithError}
import dev.argon.util.async.ErrorWrapper
import zio.*
import zio.stream.*

import java.io.IOException
import scala.reflect.TypeTest

private[backend] object BinaryResourceWrap {

  def wrap[E >: IOException](res: BinaryResource[E])(using ew: ErrorWrapper[E], rt: Runtime[Any]): scalaApi.BinaryResource[ew.EX] =
    new scalaApi.BinaryResource[ew.EX] {
      override val fileName: Option[String] = res.fileName

      override def asInputStream: IO[ew.EX, InputStreamWithError[WrappingIOException[ew.EX]]] =
        given ew2: ErrorWrapper[E]:
          override type EX = WrappingIOException[ew.EX]

          override lazy val exceptionTypeTest: TypeTest[Any, WrappingIOException[ew.EX]] =
            new TypeTest[Any, WrappingIOException[ew.EX]] {
              override def unapply(x: Any): Option[x.type & WrappingIOException[ew.EX]] =
                x.asInstanceOf[Matchable] match {
                  case WrappingIOException(_: ew.EX) => Some(x.asInstanceOf[x.type & WrappingIOException[ew.EX]])
                  case _ => None
                }
            }

          override def wrap(error: Cause[E]): WrappingIOException[ew.EX] =
            WrappingIOException(ew.wrap(error))

          override def unwrap(ex: WrappingIOException[ew.EX]): Cause[E] =
            ew.unwrap(ex.value)
        end ew2

        for
          scope <- Scope.make
          is <- res.asInputStream(using ew2).provideEnvironment(ZEnvironment(scope))
            .mapError(_.value)
        yield new InputStreamWithError[WrappingIOException[ew.EX]] {
          override def read(): Int = is.read()
          override def read(b: Array[Byte]): Int = is.read(b)
          override def read(b: Array[Byte], off: Int, len: Int): Int = is.read(b, off, len)
          override def close(): Unit =
            Unsafe.unsafely {
              rt.unsafe.run(scope.close(Exit.unit)).getOrThrow()
            }
            is.close()
          end close
        }
      end asInputStream


      override def asBytes: Stream[ew.EX, Byte] =
        ErrorWrapper.wrapStream(res.asBytes)

    }

  def unwrap[E](using ew: ErrorWrapper[E])(res: scalaApi.BinaryResource[ew.EX]): BinaryResource[E] =
    new BinaryResource[E] {
      override def fileName: Option[String] = res.fileName

      override def asInputStream[E1 >: E](using ew2: ErrorWrapper[E1] {type EX <: IOException}): ZIO[Scope, ew2.EX, InputStreamWithError[ew2.EX]] =
        ZIO.fromAutoCloseable(
            res.asInputStream
              .mapError(ex => ew2.wrap(ew.unwrap(ex)))
              .map { is =>
                new InputStreamWithError[ew2.EX] {
                  override def read(): Int =
                    try is.read()
                    catch {
                      case WrappingIOException(ex: ew2.EX) => throw ex
                    }

                  override def read(b: Array[Byte]): Int =
                    try is.read(b)
                    catch {
                      case WrappingIOException(ex: ew2.EX) => throw ex
                    }

                  override def read(b: Array[Byte], off: Int, len: Int): Int =
                    try is.read(b, off, len)
                    catch {
                      case WrappingIOException(ex: ew2.EX) => throw ex
                    }

                  override def close(): Unit =
                    try is.close()
                    catch {
                      case WrappingIOException(ex: ew2.EX) => throw ex
                    }
                }
              }
          )

      override def asBytes: Stream[E, Byte] =
        ErrorWrapper.unwrapStream(res.asBytes)
    }


}
