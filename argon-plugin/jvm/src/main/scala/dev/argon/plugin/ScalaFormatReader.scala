package dev.argon.plugin

import zio.*
import java.io.IOException
import java.io.InputStream

private[plugin] final class ScalaFormatReader(inputStream: InputStream) extends dev.argon.verilization.runtime.zio.FormatReader {
  def readByte(): IO[IOException, Byte] =
    ZIO.succeed {
      val b = inputStream.readNBytes(1)
      b(0)
    }

  def readShort(): IO[IOException, Short] =
    ZIO.succeed {
      val b = inputStream.readNBytes(2)
      ((b(0) & 0xFF) | (b(1) & 0xFF) << 8).toShort
    }
    
  def readInt(): IO[IOException, Int] =
    ZIO.succeed {
      val b = inputStream.readNBytes(4)
      (b(0) & 0xFF) | (b(1) & 0xFF) << 8 | (b(2) & 0xFF) << 16 | (b(3) & 0xFF) << 24
    }

  def readLong(): IO[IOException, Long] =
    ZIO.succeed {
      val b = inputStream.readNBytes(8)
      (b(0) & 0xFFL) | (b(1) & 0xFFL) << 8 | (b(2) & 0xFFL) << 16 | (b(3) & 0xFFL) << 24 |
        (b(4) & 0xFFL) << 32 | (b(5) & 0xFFL) << 40 | (b(6) & 0xFFL) << 48 | (b(7) & 0xFFL) << 56
    }

  def readBytes(count: Int): IO[IOException, Chunk[Byte]] =
    ZIO.succeed {
      Chunk.fromArray(inputStream.readNBytes(count))
    }
}
