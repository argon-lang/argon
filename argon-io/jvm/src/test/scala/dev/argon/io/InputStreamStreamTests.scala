package dev.argon.io

import java.io.{ByteArrayInputStream, InputStream}

import dev.argon.stream.{ArStream, Resource, StreamTransformation}
import zio._
import zio.interop.catz._
import zio.blocking.Blocking
import zio.test._
import zio.test.Assertion._

import scala.collection.mutable.ArrayBuffer

object InputStreamStreamTests extends DefaultRunnableSpec({
  import StreamCommon._

  def createInputStreamStream: RIO[Blocking, InputStreamStream[Blocking, Throwable]] =
    ZIO.access[Blocking] { res =>
      InputStreamStream[Blocking, Throwable](identity)(
        ZManaged.fromAutoCloseable(
          IO.effectTotal { sampleInputStream }
        )
      )
    }

  suite("InputStreamStreamTests")(
    testM("readDirectly using single byte read") {
      assertM(createInputStreamStream.flatMap { InputStreamReaderTransformation(_)(usingSingleByteRead) }.orDie, equalTo(streamContent))
    },
    testM("readDirectly using buffer reads") {
      assertM(createInputStreamStream.flatMap { InputStreamReaderTransformation(_)(usingBufferReader) }.orDie, equalTo(streamContent))
    },
    testM("transform stream using toVector") {
      assertM(createInputStreamStream.flatMap { _.runCollect }.map { l => l.flatMap(_.toSeq) }.orDie, equalTo(streamContent))
    },
  )

})
