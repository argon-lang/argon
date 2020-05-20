package dev.argon.platform

import dev.argon.platform.StreamCommon._
import zio.ZIO
import zio.blocking.Blocking
import zio.random.Random
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._

object InputStreamReaderTransformationTests extends DefaultRunnableSpec {
  override def spec: ZSpec[Environment, Failure] =
    suite("InputStreamReaderTransformationTests")(
      testM("single byte read") {
        checkM(Gen.listOf[Blocking with Random with Sized, Byte](Gen.anyByte)) { byteList =>
          assertM(
            InputStreamReaderTransformation[Blocking, Throwable, List[Byte]](ZStream.fromIterable(byteList)) { inputStream =>
              ZIO.accessM[Blocking](_.get.effectBlocking {

                def readBytes(l: List[Byte]): List[Byte] =
                  inputStream.read() match {
                    case b if b < 0 => l
                    case b => readBytes(l :+ b.toByte)
                  }

                readBytes(Nil)
              }.orDie)
            }
          )(equalTo(byteList))
        }
      },
    )
}
