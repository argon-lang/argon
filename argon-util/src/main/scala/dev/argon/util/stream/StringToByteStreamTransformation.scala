package dev.argon.util.stream

import java.nio.charset.Charset

import cats.Monad

object StringToByteStreamTransformation {

  def apply[F[-_, +_, +_], R, E, X](charsest: Charset)(implicit monadInstance: Monad[F[R, E, ?]]): StreamTransformation[F, R, E, String, X, Byte, X] =
    StreamTransformation.identity[F, R, E, String, X]
      .map { s => s.getBytes(charsest).toVector }
      .into(StreamTransformation.flattenVector[F, R, E, Byte, X])

}
