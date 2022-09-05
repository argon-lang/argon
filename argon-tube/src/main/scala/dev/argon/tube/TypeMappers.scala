package dev.argon.tube

import com.google.protobuf.ByteString
import scalapb.TypeMapper
import zio.{Chunk, ChunkBuilder}

given TypeMapper[ByteString, Chunk[Byte]] with
  override def toCustom(base: ByteString): Chunk[Byte] =
    Chunk.fromArray(base.toByteArray())
    
  override def toBase(custom: Chunk[Byte]): ByteString = {
    val bso = ByteString.newOutput(custom.size)
    for i <- custom.indices do
      bso.write(custom.byte(i))
    end for
    bso.toByteString()
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

given TypeMapper[dev.argon.tube.OffsetDateTime, java.time.OffsetDateTime] with
  override def toCustom(base: dev.argon.tube.OffsetDateTime): java.time.OffsetDateTime =
    java.time.OffsetDateTime.of(
      base.year,
      base.month,
      base.dayOfMonth,
      base.hour,
      base.minute,
      base.second,
      base.nanoOfSecond,
      java.time.ZoneOffset.ofTotalSeconds(base.offset)
    )

  override def toBase(custom: java.time.OffsetDateTime): dev.argon.tube.OffsetDateTime =
    dev.argon.tube.OffsetDateTime(
      year = custom.getYear,
      month = custom.getMonthValue,
      dayOfMonth = custom.getDayOfMonth,
      hour = custom.getHour,
      minute = custom.getMinute,
      second = custom.getSecond,
      nanoOfSecond = custom.getNano,
      offset = custom.getOffset.getTotalSeconds,
    )
end given

given TypeMapper[dev.argon.tube.LocalDateTime, java.time.LocalDateTime] with
  override def toCustom(base: dev.argon.tube.LocalDateTime): java.time.LocalDateTime =
    java.time.LocalDateTime.of(
      base.year,
      base.month,
      base.dayOfMonth,
      base.hour,
      base.minute,
      base.second,
      base.nanoOfSecond,
    )

  override def toBase(custom: java.time.LocalDateTime): dev.argon.tube.LocalDateTime =
    dev.argon.tube.LocalDateTime(
      year = custom.getYear,
      month = custom.getMonthValue,
      dayOfMonth = custom.getDayOfMonth,
      hour = custom.getHour,
      minute = custom.getMinute,
      second = custom.getSecond,
      nanoOfSecond = custom.getNano,
    )
end given

given TypeMapper[dev.argon.tube.LocalDate, java.time.LocalDate] with
  override def toCustom(base: dev.argon.tube.LocalDate): java.time.LocalDate =
    java.time.LocalDate.of(
      base.year,
      base.month,
      base.dayOfMonth,
    )

  override def toBase(custom: java.time.LocalDate): dev.argon.tube.LocalDate =
    dev.argon.tube.LocalDate(
      year = custom.getYear,
      month = custom.getMonthValue,
      dayOfMonth = custom.getDayOfMonth,
    )
end given

given TypeMapper[dev.argon.tube.LocalTime, java.time.LocalTime] with
  override def toCustom(base: dev.argon.tube.LocalTime): java.time.LocalTime =
    java.time.LocalTime.of(
      base.hour,
      base.minute,
      base.second,
      base.nanoOfSecond,
    )

  override def toBase(custom: java.time.LocalTime): dev.argon.tube.LocalTime =
    dev.argon.tube.LocalTime(
      hour = custom.getHour,
      minute = custom.getMinute,
      second = custom.getSecond,
      nanoOfSecond = custom.getNano,
    )
end given




