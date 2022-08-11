package dev.argon.tube

import com.google.protobuf.ByteString
import scalapb.TypeMapper
import zio.{Chunk, ChunkBuilder}

given TypeMapper[ByteString, Chunk[Byte]] with
  override def toCustom(base: ByteString): Chunk[Byte] = {
    val cb = ChunkBuilder.Byte()
    cb.sizeHint(base.size())
    for i <- 0 until base.size() do
      cb += base.byteAt(i)
    end for
    cb.result()
  }
  override def toBase(custom: Chunk[Byte]): ByteString = {
    val bso = ByteString.newOutput(custom.size)
    for i <- custom.indices do
      bso.write(custom.byte(i))
    end for
    bso.toByteString
  }
end given

given TypeMapper[dev.argon.tube.BigInt, scala.math.BigInt] with
  override def toCustom(base: dev.argon.tube.BigInt): scala.math.BigInt =
    base.value match {
      case dev.argon.tube.BigInt.Value.UintValue(n) => n
      case dev.argon.tube.BigInt.Value.SintValue(n) => n
      case dev.argon.tube.BigInt.Value.BigIntValue(b) => scala.math.BigInt(b.toArray)
      case _: dev.argon.tube.BigInt.Value.Empty.type => 0
    }

  override def toBase(custom: scala.math.BigInt): dev.argon.tube.BigInt =
    dev.argon.tube.BigInt(
      if custom == 0 then
        dev.argon.tube.BigInt.Value.Empty
      else if custom > 0 && custom < (scala.math.BigInt(1) << 64) then
        dev.argon.tube.BigInt.Value.UintValue(custom.toLong)
      else if custom < 0 && custom >= Long.MinValue then
        dev.argon.tube.BigInt.Value.SintValue(custom.toLong)
      else
        dev.argon.tube.BigInt.Value.BigIntValue(Chunk.fromArray(custom.toByteArray))
    )
end given


