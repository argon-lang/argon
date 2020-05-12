package dev.argon.stream

import java.nio.charset.Charset

import cats._
import cats.implicits._
import dev.argon.stream.builder.Source
import zio.Chunk

object StringToByteStreamTransformation {

  def convert[R, E, X](charset: Charset)(text: Source[R, E, String, X]): Source[R, E, Chunk[Byte], X] =
    text.map { s => Chunk.fromArray(s.getBytes(charset)) }

}
