package dev.argon.backend

import dev.argon.io.BinaryResource
import dev.argon.util.async.ErrorWrapper
import zio.*
import zio.stream.*

import scala.reflect.TypeTest

private[backend] object BinaryResourceWrap {

  def wrap[E](res: BinaryResource[E])(using ew: ErrorWrapper[E]): scalaApi.BinaryResource[ew.EX] =
    new scalaApi.BinaryResource[ew.EX] {
      override val fileName: Option[String] = res.fileName

      override def asBytes: Stream[ew.EX, Byte] =
        ErrorWrapper.wrapStream(res.asBytes)
    }

  def unwrap[E](using ew: ErrorWrapper[E])(res: scalaApi.BinaryResource[ew.EX]): BinaryResource[E] =
    new BinaryResource[E] {
      override def fileName: Option[String] = res.fileName

      override def asBytes: Stream[E, Byte] =
        ErrorWrapper.unwrapStream(res.asBytes)
    }


}
