package dev.argon.plugin

import dev.argon.verilization.runtime.zio.FormatWriter
import zio.*
import java.io.OutputStream
import java.io.IOException
import zio.Chunk

private[plugin] final class ScalaFormatWriter(output: OutputStream) extends FormatWriter {
  def writeByte(b: Byte): IO[IOException, Unit] =
    ZIO.succeed {
      output.write(b)
    }

  def writeShort(s: Short): IO[IOException, Unit] =
    ZIO.succeed {
      output.write(s)
      output.write(s >> 8)
    }

  def writeInt(i: Int): IO[IOException, Unit] =
    ZIO.succeed {
      output.write(i)
      output.write(i >> 8)
      output.write(i >> 16)
      output.write(i >> 24)
    }

  def writeLong(l: Long): IO[IOException, Unit] =
    ZIO.succeed {
      output.write(l.toInt)
      output.write((l >> 8).toInt)
      output.write((l >> 16).toInt)
      output.write((l >> 24).toInt)
      output.write((l >> 32).toInt)
      output.write((l >> 40).toInt)
      output.write((l >> 48).toInt)
      output.write((l >> 56).toInt)
    }

  def writeBytes(data: Chunk[Byte]): IO[IOException, Unit] =
    ZIO.succeed {
      output.write(data.toArray)
    }

  def flush: IO[IOException, Unit] =
    ZIO.succeed {
      output.flush()
    }

}
