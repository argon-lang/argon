package dev.argon.io

import java.io.{ByteArrayInputStream, InputStream}

import org.scalatest.{FlatSpec, Matchers}
import zio._
import zio.blocking.Blocking

import scala.collection.mutable.ArrayBuffer

class InputStreamReaderTransformationTests extends FlatSpec with Matchers with DefaultRuntime {

  private val streamContent = Seq[Byte](0, 7, 5, 9, 4, -1)


  "InputStreamReaderTransformation" should "readDirectly using read()" in {
    unsafeRun(InputStreamReaderTransformation { inputStream =>
      ZIO.accessM[Blocking] { _.blocking.effectBlocking {
        for (b <- streamContent) {
          inputStream.read() shouldBe (b & 0xFF)
        }

        inputStream.read() shouldBe (-1)
      } }
    }.readDirectly[Blocking, Throwable](new ByteArrayInputStream(streamContent.toArray)))
  }

  it should "readDirectly using read(Array[Byte], Int, Int)" in {
    unsafeRun(InputStreamReaderTransformation { inputStream =>
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

        allData.toSeq shouldBe streamContent
      } }
    }.readDirectly[Blocking, Throwable](new ByteArrayInputStream(streamContent.toArray)))
  }



}
