package dev.argon.platform

import java.io.{InputStream, OutputStream}

import zio._
import zio.blocking.Blocking

import scala.annotation.unused
import scala.collection.mutable.ArrayBuffer

@SuppressWarnings(Array("scalafix:DisableSyntax.var", "scalafix:DisableSyntax.while"))
object StreamCommon {
  def noReads(@unused inputStream: InputStream): RIO[Blocking, Unit] =
    IO.succeed(())

  def usingSingleByteRead(inputStream: InputStream): RIO[Blocking, List[Byte]] =
    ZIO.accessM[Blocking] { _.get.effectBlockingInterrupt {
      val allData = new ArrayBuffer[Byte]()

      def readMore(): Unit = {
        val b = inputStream.read()
        if(b >= 0) {
          allData += b.toByte
          readMore()
        }
      }

      readMore()

      allData.toList
    } }

  val retryCount = 10

  def usingSingleByteReadExtra(inputStream: InputStream): RIO[Blocking, Int] =
    ZIO.accessM[Blocking] { _.get.effectBlockingInterrupt {
      var eofCount = 0
      while(inputStream.read() >= 0) {}
      for(_ <- 0 until retryCount) {
        if(inputStream.read() < 0) {
          eofCount += 1
        }
      }
      eofCount
    } }

  def usingBufferReader(inputStream: InputStream): RIO[Blocking, List[Byte]] =
    ZIO.accessM[Blocking] { _.get.effectBlockingInterrupt {
      val allData = new ArrayBuffer[Byte]()

      val buff = new Array[Byte](1024)
      def readMore(): Unit = {
        val bytesRead = inputStream.read(buff, 0, buff.length)
        if(bytesRead > 0) {
          allData ++= buff.take(bytesRead)
          readMore()
        }
      }

      readMore()

      allData.toList
    } }

  def usingBufferReaderExtra(inputStream: InputStream): RIO[Blocking, Int] =
    ZIO.accessM[Blocking] { _.get.effectBlockingInterrupt {
      var eofCount = 0
      val buff = new Array[Byte](1024)
      while(inputStream.read(buff, 0, buff.length) >= 0) {}
      for(_ <- 0 until retryCount) {
        if(inputStream.read(buff, 0, buff.length) < 0) {
          eofCount += 1
        }
      }
      eofCount
    } }

  def usingSingleByteWrite(data: Chunk[Byte])(outputStream: OutputStream): RIO[Blocking, Unit] =
    ZIO.accessM[Blocking] { _.get.effectBlockingInterrupt {
      for(b <- data) {
        outputStream.write(b.toInt)
      }
    } }

  def usingBufferWriter(data: Chunk[Byte])(outputStream: OutputStream): RIO[Blocking, Unit] =
    ZIO.accessM[Blocking] { _.get.effectBlockingInterrupt {
      outputStream.write(data.toArray)
    } }
}
