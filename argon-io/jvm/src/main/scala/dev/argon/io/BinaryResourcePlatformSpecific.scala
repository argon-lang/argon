package dev.argon.io

import dev.argon.nobleidl.runtime.util.InputStreamWithError
import dev.argon.util.async.ErrorWrapper
import zio.*

import java.io.{IOException, InputStream}

trait BinaryResourcePlatformSpecific[+E] {
  self: BinaryResource[E] =>

  def asInputStream[E1 >: E](using ew: ErrorWrapper[E1] { type EX <: IOException }): ZIO[Scope, ew.EX, InputStreamWithError[ew.EX]] =
    ErrorWrapper.wrapStream[Any, E1, Byte](asBytes).toInputStream
      .map(inputStreamToTypedUnsafe)

  private def inputStreamToTypedUnsafe[EX <: IOException](is: InputStream): InputStreamWithError[EX] =
    new InputStreamWithError[EX] {
      override def read(): Int = is.read()
      override def read(b: Array[Byte]): Int = is.read(b)
      override def read(b: Array[Byte], off: Int, len: Int): Int = is.read(b, off, len)
      override def close(): Unit = is.close()
    }
}
