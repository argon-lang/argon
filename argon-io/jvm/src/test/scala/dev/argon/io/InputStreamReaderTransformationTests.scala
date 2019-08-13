package dev.argon.io

import java.io.{ByteArrayInputStream, InputStream}

import dev.argon.stream.ArStream
import org.scalatest.{FlatSpec, Matchers}
import zio._
import zio.interop.catz._
import zio.blocking.Blocking

import scala.collection.mutable.ArrayBuffer

class InputStreamReaderTransformationTests extends FlatSpec with Matchers with DefaultRuntime with StreamCommon {

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

  it should "handle extra single byte reads" in {
    unsafeRun(sampleArStream.foldLeft(InputStreamReaderTransformation(usingSingleByteReadExtra))) shouldBe retryCount
  }

  it should "handle extra buffer reads" in {
    unsafeRun(sampleArStream.foldLeft(InputStreamReaderTransformation(usingBufferReaderExtra))) shouldBe retryCount
  }

}
