package dev.argon.io

import java.io.OutputStream

import dev.argon.stream.StreamTransformation
import org.scalatest.{FlatSpec, Matchers}
import zio._
import zio.blocking.Blocking
import zio.interop.catz._

class OutputStreamWriterStreamTests extends FlatSpec with Matchers with DefaultRuntime with StreamCommon {

  def createStream(f: OutputStream => ZIO[Blocking, Throwable, Unit]): RIO[Blocking, OutputStreamWriterStream[Blocking, Throwable]] =
    ZIO.access[Blocking] { env =>
      OutputStreamWriterStream(env.blocking)(f)
    }

  "OutputStreamWriterStream" should "stream using single byte write" in {
    unsafeRun(createStream(usingSingleByteWrite).flatMap { _.foldLeft(StreamTransformation.toVector[ZIO, Blocking, Throwable, Byte]) }) shouldBe streamContent
  }

  it should "stream using buffer writes" in {
    unsafeRun(createStream(usingBufferWriter).flatMap { _.foldLeft(StreamTransformation.toVector[ZIO, Blocking, Throwable, Byte]) }) shouldBe streamContent
  }

}
