package dev.argon.io

import java.io.{ByteArrayInputStream, InputStream}

import org.scalatest.{FlatSpec, Matchers}
import zio._
import zio.blocking.Blocking

import scala.collection.mutable.ArrayBuffer

class InputStreamReaderTransformationTests extends FlatSpec with Matchers with DefaultRuntime {

  private val streamContent = Seq[Byte](0, 7, 5, 9, 4, -1)

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
    unsafeRun(InputStreamReaderTransformation(usingSingleByteRead).readDirectly(new ByteArrayInputStream(streamContent.toArray))) shouldBe streamContent
  }

  it should "readDirectly using buffer reads" in {
    unsafeRun(InputStreamReaderTransformation(usingBufferReader).readDirectly(new ByteArrayInputStream(streamContent.toArray))) shouldBe streamContent
  }



}
