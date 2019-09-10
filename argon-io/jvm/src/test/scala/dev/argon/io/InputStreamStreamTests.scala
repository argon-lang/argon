package dev.argon.io

import java.io.{ByteArrayInputStream, InputStream}

import dev.argon.stream.{ArStream, Resource, StreamTransformation}
import org.scalatest.{FlatSpec, Matchers}
import zio._
import zio.interop.catz._
import zio.blocking.Blocking

import scala.collection.mutable.ArrayBuffer

@SuppressWarnings(Array("org.wartremover.warts.Var"))
class InputStreamStreamTests extends FlatSpec with Matchers with DefaultRuntime with StreamCommon {

  private def createInputStreamStream: RIO[Blocking, InputStreamStream[Blocking, Throwable]] =
    ZIO.access[Blocking] { res =>
      InputStreamStream[Blocking, Throwable](identity)(
        ZManaged.fromAutoCloseable(
          IO.effectTotal { sampleInputStream }
        )
      )
    }

  "InputStreamStream" should "readDirectly using single byte read" in {
    unsafeRun(createInputStreamStream.flatMap { InputStreamReaderTransformation(_)(usingSingleByteRead) }) shouldBe streamContent
  }

  it should "readDirectly using buffer reads" in {
    unsafeRun(createInputStreamStream.flatMap { InputStreamReaderTransformation(_)(usingBufferReader) }) shouldBe streamContent
  }

  it should "transform stream using toVector" in {
    unsafeRun(createInputStreamStream.flatMap { _.runCollect }) shouldBe streamContent
  }

}
