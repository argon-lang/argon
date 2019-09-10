package dev.argon.io

import java.io.{ByteArrayInputStream, InputStream}

import dev.argon.stream.ArStream
import org.scalatest.{FlatSpec, Matchers}
import zio._
import zio.interop.catz._
import zio.blocking.Blocking
import zio.stream.ZStream

import scala.collection.mutable.ArrayBuffer

class InputStreamReaderTransformationTests extends FlatSpec with Matchers with DefaultRuntime with StreamCommon {

  "InputStreamReaderTransformation" should "transform stream using single byte read" in {
    unsafeRun(InputStreamReaderTransformation(sampleZStream)(usingSingleByteRead)) shouldBe streamContent
  }

  it should "transform stream using buffer reads" in {
    unsafeRun(InputStreamReaderTransformation(sampleZStream)(usingBufferReader)) shouldBe streamContent
  }

  it should "handle extra single byte reads" in {
    unsafeRun(InputStreamReaderTransformation(sampleZStream)(usingSingleByteReadExtra)) shouldBe retryCount
  }

  it should "handle extra buffer reads" in {
    unsafeRun(InputStreamReaderTransformation(sampleZStream)(usingBufferReaderExtra)) shouldBe retryCount
  }

  it should "handle empty single byte reads" in {
    unsafeRun(InputStreamReaderTransformation(emptyZStream)(usingSingleByteRead)) shouldBe Seq()
  }

  it should "handle empty buffer reads" in {
    unsafeRun(InputStreamReaderTransformation(emptyZStream)(usingBufferReader)) shouldBe Seq()
  }

  it should "handle no reads" in {
    unsafeRun(InputStreamReaderTransformation(emptyZStream)(noReads)) shouldBe Seq()
  }

}
