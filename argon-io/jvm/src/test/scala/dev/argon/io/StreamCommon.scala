package dev.argon.io

import java.io.{ByteArrayInputStream, InputStream, OutputStream}

import dev.argon.stream.ArStream
import zio._
import zio.blocking.Blocking

import scala.collection.mutable.ArrayBuffer

@SuppressWarnings(Array("org.wartremover.warts.Var"))
trait StreamCommon {

  val streamContent = Seq[Byte](0, 7, 5, 9, 4, -1)
  def sampleInputStream: InputStream = new ByteArrayInputStream(streamContent.toArray)
  def sampleArStream: ArStream[ZIO, Blocking, Throwable, Byte] = ArStream.fromVector(streamContent.toVector)
  def emptyArStream: ArStream[ZIO, Blocking, Throwable, Byte] = ArStream.fromVector(Vector.empty)

  def noReads(inputStream: InputStream): RIO[Blocking, Unit] =
    IO.succeed(())

  def usingSingleByteRead(inputStream: InputStream): RIO[Blocking, Seq[Byte]] =
    ZIO.accessM[Blocking] { _.blocking.effectBlocking {
      val allData = new ArrayBuffer[Byte]()

      def readMore(): Unit = {
        val b = inputStream.read()
        if(b >= 0) {
          allData += b.toByte
          readMore()
        }
      }

      readMore()

      allData.toSeq
    } }

  val retryCount = 10

  def usingSingleByteReadExtra(inputStream: InputStream): RIO[Blocking, Int] =
    ZIO.accessM[Blocking] { _.blocking.effectBlocking {
      var eofCount = 0
      while(inputStream.read() >= 0) {}
      for(_ <- 0 until retryCount) {
        if(inputStream.read() < 0) {
          eofCount += 1
        }
      }
      eofCount
    } }

  def usingBufferReader(inputStream: InputStream): RIO[Blocking, Seq[Byte]] =
    ZIO.accessM[Blocking] { _.blocking.effectBlocking {
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

      allData.toSeq
    } }

  def usingBufferReaderExtra(inputStream: InputStream): RIO[Blocking, Int] =
    ZIO.accessM[Blocking] { _.blocking.effectBlocking {
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

  def usingSingleByteWrite(outputStream: OutputStream): RIO[Blocking, Unit] =
    ZIO.accessM[Blocking] { _.blocking.effectBlocking {
      for(b <- streamContent) {
        outputStream.write(b.toInt)
      }
    } }
}
