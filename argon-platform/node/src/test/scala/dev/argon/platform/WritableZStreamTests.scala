package dev.argon.platform

import zio._
import zio.test._
import zio.test.Assertion._
import cats.implicits._
import zio.interop.catz.core._

import scalajs.js
import scalajs.js.JSConverters._

object WritableZStreamTests extends DefaultRunnableSpec {

  @SuppressWarnings(Array("dev.argon.warts.ZioEffect"))
  def writeDataToWritable(writable: NodeWritable, data: Chunk[Byte]): IO[js.Error, Unit] =
    IO.effectAsync { register =>
      val buffer = NodeBuffer.from(data.toArray.toJSArray.map(_.toInt))
      writable.write(buffer, "utf8", _ => {
        register(IO.succeed(()))
      })
    }

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    suite("WritableZStream")(
      testM("Single Chunk") {
        checkM(Gen.chunkOf(Gen.anyByte)) { byteList =>
          assertM(
            WritableZStream { writable =>
              writeDataToWritable(writable, byteList)
            }
              .runCollect
              .mapError(js.JavaScriptException)
              .orDie
          )(
            equalTo(byteList)
          )
        }
      },

      testM("Chunk per byte") {
        checkM(Gen.chunkOf(Gen.anyByte)) { byteList =>
          assertM(
            WritableZStream { writable =>
              ZIO.foreach_(byteList) { b =>
                writeDataToWritable(writable, Chunk(b))
              }
            }
              .runCollect
              .mapError(js.JavaScriptException)
              .orDie
          )(
            equalTo(byteList)
          )
        }
      },

    )
}
