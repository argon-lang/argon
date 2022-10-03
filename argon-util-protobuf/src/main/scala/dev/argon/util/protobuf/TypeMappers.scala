package dev.argon.util.protobuf

import com.google.protobuf.ByteString
import scalapb.TypeMapper
import zio.{Chunk, ChunkBuilder}

given TypeMapper[ByteString, Chunk[Byte]] with
  override def toCustom(base: ByteString): Chunk[Byte] =
    Chunk.fromArray(base.toByteArray().nn)

  override def toBase(custom: Chunk[Byte]): ByteString = {
    val bso = ByteString.newOutput(custom.size).nn
    for i <- custom.indices do
      bso.write(custom.byte(i))
    end for
    bso.toByteString().nn
  }
end given

