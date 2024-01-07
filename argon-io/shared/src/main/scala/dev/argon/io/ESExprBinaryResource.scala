package dev.argon.io

import dev.argon.esexpr.ESExprCodec
import zio.*
import zio.stream.*

trait ESExprBinaryResource[R, E, A] extends BinaryResource[R, E] {
  protected def codec: ESExprCodec[A]

  def decoded: ZIO[R, E, A]
}

object ESExprBinaryResource {
  given resourceCodec[A](using ESExprCodec[A]): BinaryResourceDecoder[[R, E] =>> ESExprBinaryResource[R, E, A], Any, Nothing] with
    override def decode[R <: Any, E >: Nothing](resource: BinaryResource[R, E]): ESExprBinaryResource[R, E, A] =
      new ESExprBinaryResource[R, E, A] {
        override protected def codec: ESExprCodec[A] =
          summon[ESExprCodec[A]]

        override def decoded: ZIO[R, E, A] = ???

        override def asBytes: ZStream[R, E, Byte] =
          resource.asBytes

        override def fileName: Option[String] =
          resource.fileName
      }
  end resourceCodec
}

