package dev.argon.io

import java.io.{ByteArrayInputStream, InputStream}

import zio._
import zio.interop.catz._
import zio.blocking.Blocking
import zio.random.Random
import zio.test._
import zio.test.Assertion._

import scala.collection.mutable.ArrayBuffer

object InputStreamStreamTests extends DefaultRunnableSpec({
  import StreamCommon._

  def createInputStreamStream(data: List[Byte]): RIO[Blocking, InputStreamStream[Blocking, Throwable]] =
    ZIO.access[Blocking] { res =>
      InputStreamStream[Blocking, Throwable](identity)(
        ZManaged.fromAutoCloseable(
          IO.effectTotal { new ByteArrayInputStream(data.toArray) }
        )
      )
    }

  suite("InputStreamStreamTests")(
    testM("readDirectly using single byte read") {
      checkM(Gen.listOf[Blocking with Random with Sized, Byte](Gen.anyByte)) { byteList =>
        assertM(createInputStreamStream(byteList).flatMap { InputStreamReaderTransformation(_)(usingSingleByteRead) }.orDie, equalTo(byteList))
      }
    },
    testM("readDirectly using buffer reads") {
      checkM(Gen.listOf[Blocking with Random with Sized, Byte](Gen.anyByte)) { byteList =>
        assertM(createInputStreamStream(byteList).flatMap { InputStreamReaderTransformation(_)(usingBufferReader) }.orDie, equalTo(byteList))
      }
    },
    testM("transform stream using toVector") {
      checkM(Gen.listOf[Blocking with Random with Sized, Byte](Gen.anyByte)) { byteList =>
        assertM(createInputStreamStream(byteList).flatMap { _.runCollect }.map { l => l.flatMap(_.toSeq.toList) }.orDie, equalTo(byteList))
      }
    },
  )

})
