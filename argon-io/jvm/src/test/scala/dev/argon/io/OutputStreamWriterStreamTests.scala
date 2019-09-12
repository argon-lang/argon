package dev.argon.io

import zio.test._
import zio.test.Assertion._
import StreamCommon._

object OutputStreamWriterStreamTests extends DefaultRunnableSpec(
  suite("OutputStreamWriterStreamTests")(
    testM("stream using single byte write") {
      assertM(OutputStreamWriterStream(usingSingleByteWrite).runCollect.map { l => l.flatMap(_.toSeq) }.orDie, equalTo(streamContent))
    },
    testM("stream using buffer writes") {
      assertM(OutputStreamWriterStream(usingBufferWriter).runCollect.map { l => l.flatMap(_.toSeq) }.orDie, equalTo(streamContent))
    },
  )
)
