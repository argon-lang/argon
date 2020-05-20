package dev.argon.stream

import java.nio.charset.Charset

import cats._
import cats.implicits._
import dev.argon.stream.builder.Source
import zio.Chunk
import zio.stream.ZStream

object StringToByteStreamTransformation {

  def convert[R, E, X](charset: Charset)(text: ZStream[R, E, String]): ZStream[R, E, Byte] =
    text.mapChunks { _.flatMap { s => Chunk.fromArray(s.getBytes(charset)) } }

}
