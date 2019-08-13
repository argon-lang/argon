package dev.argon.io

import java.io.{ByteArrayInputStream, InputStream}

import dev.argon.stream.ArStream
import org.scalatest.{FlatSpec, Matchers}
import zio._
import zio.interop.catz._
import zio.blocking.Blocking

import scala.collection.mutable.ArrayBuffer

@SuppressWarnings(Array("org.wartremover.warts.Var"))
class InputStreamReaderTransformationTests extends FlatSpec with Matchers with DefaultRuntime {

  private val streamContent = Seq[Byte](0, 7, 5, 9, 4, -1)
  private def sampleInputStream: InputStream = new ByteArrayInputStream(streamContent.toArray)
  private def sampleArStream: ArStream[ZIO, Blocking, Throwable, Byte] = ArStream.fromVector(streamContent.toVector)

  private def usingSingleByteRead(inputStream: InputStream): RIO[Blocking, Seq[Byte]] =
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

  private def usingBufferReader(inputStream: InputStream): RIO[Blocking, Seq[Byte]] =
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

  "InputStreamReaderTransformation" should "readDirectly using single byte read" in {
    unsafeRun(InputStreamReaderTransformation(usingSingleByteRead).readDirectly(sampleInputStream)) shouldBe streamContent
  }

  it should "readDirectly using buffer reads" in {
    unsafeRun(InputStreamReaderTransformation(usingBufferReader).readDirectly(sampleInputStream)) shouldBe streamContent
  }

  it should "transform stream using single byte read" in {
    unsafeRun(sampleArStream.foldLeft(InputStreamReaderTransformation(usingSingleByteRead))) shouldBe streamContent
  }

  it should "transform stream using buffer reads" in {
    unsafeRun(sampleArStream.foldLeft(InputStreamReaderTransformation(usingBufferReader))) shouldBe streamContent
  }

  val retryCount = 10

  it should "handle extra single byte reads" in {
    unsafeRun(sampleArStream.foldLeft(InputStreamReaderTransformation { inputStream =>
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
    })) shouldBe retryCount
  }

  it should "handle extra buffer reads" in {
    unsafeRun(sampleArStream.foldLeft(InputStreamReaderTransformation { inputStream =>
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
    })) shouldBe retryCount
  }

}
