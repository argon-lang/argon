package dev.argon.io

import java.io.OutputStream

import dev.argon.stream.StreamTransformation
import org.scalatest.{FlatSpec, Matchers}
import zio._
import zio.blocking.Blocking
import zio.interop.catz._
import zio.stream.ZStream

class OutputStreamWriterStreamTests extends FlatSpec with Matchers with DefaultRuntime with StreamCommon {

  "OutputStreamWriterStream" should "stream using single byte write" in {
    unsafeRun(OutputStreamWriterStream(usingSingleByteWrite).runCollect) shouldBe streamContent
  }

  it should "stream using buffer writes" in {
    unsafeRun(OutputStreamWriterStream(usingBufferWriter).runCollect) shouldBe streamContent
  }

}
