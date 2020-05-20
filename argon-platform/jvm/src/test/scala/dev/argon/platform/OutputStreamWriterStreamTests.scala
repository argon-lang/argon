package dev.argon.platform

import zio.test._
import zio.test.Assertion._
import StreamCommon._
import zio.blocking.Blocking
import zio.random.Random

object OutputStreamWriterStreamTests extends DefaultRunnableSpec {
  override def spec: ZSpec[Environment, Failure] =
    suite("OutputStreamWriterStreamTests")(
      testM("stream using single byte write") {
        checkM(Gen.listOf[Blocking with Random with Sized, Byte](Gen.anyByte)) { byteList =>
          assertM(OutputStreamWriterStream(usingSingleByteWrite(byteList)).runCollect.orDie)(equalTo(byteList))
        }
      },
      testM("stream using buffer writes") {
        checkM(Gen.listOf[Blocking with Random with Sized, Byte](Gen.anyByte)) { byteList =>
          assertM(OutputStreamWriterStream(usingBufferWriter(byteList)).runCollect.orDie)(equalTo(byteList))
        }
      },
    )
}
