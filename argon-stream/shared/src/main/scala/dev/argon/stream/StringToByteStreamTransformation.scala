package dev.argon.stream

import java.nio.charset.Charset

import cats._
import cats.implicits._
import dev.argon.stream.builder.Source
import zio.Chunk

object StringToByteStreamTransformation {

  def convert[F[_]: Functor, X](charset: Charset)(text: Source[F, String, X]): Source[F, Chunk[Byte], X] =
    text.map { s => Chunk.fromArray(s.getBytes(charset)) }

}
