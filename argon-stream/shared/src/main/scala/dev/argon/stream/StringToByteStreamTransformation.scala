package dev.argon.stream

import java.nio.charset.Charset

import cats._
import cats.implicits._
import dev.argon.stream.builder.Source
import zio.Chunk

object StringToByteStreamTransformation {

  def apply[F[-_, +_, +_], R, E, X](charsest: Charset)(implicit monadInstance: Monad[F[R, E, ?]]): StreamTransformation[F, R, E, String, X, Byte, X] =
    StreamTransformation.identity[F, R, E, String, X]
      .map { s => s.getBytes(charsest).toVector }
      .into(StreamTransformation.flattenVector[F, R, E, Byte, X])

  def convert[F[_]: Functor, X](charset: Charset)(text: Source[F, String, X]): Source[F, Chunk[Byte], X] =
    text.map { s => Chunk.fromArray(s.getBytes(charset)) }

}
