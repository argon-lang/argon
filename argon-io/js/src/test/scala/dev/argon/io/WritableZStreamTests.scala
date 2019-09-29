package dev.argon.io

import zio._
import zio.test._
import zio.test.Assertion._
import cats.implicits._
import zio.interop.catz._

import scalajs.js
import scalajs.js.JSConverters._

object WritableZStreamTests extends DefaultRunnableSpec({

  def writeDataToWritable(writable: NodeWritable, data: List[Byte]): IO[js.Error, Unit] =
    IO.effectAsync { register =>
      writable.write(NodeBuffer.from(data.toArray.toJSArray.map(_.toInt)), "utf8", _ => {
        register(IO.succeed(()))
      })
    }


  suite("WritableZStream")(
    testM("Single Chunk") {
      checkM(Gen.listOf(Gen.anyByte)) { byteList =>
        assertM(
          WritableZStream { writable =>
            writeDataToWritable(writable, byteList)
          }
            .runCollect
            .mapError(js.JavaScriptException)
            .orDie,
          equalTo(List(Chunk.fromIterable(byteList)))
        )
      }
    },

    testM("Chunk per byte") {
      checkM(Gen.listOf(Gen.anyByte)) { byteList =>
        assertM(
          WritableZStream { writable =>
            byteList.traverse_ { b =>
              writeDataToWritable(writable, List(b))
            }
          }
            .runCollect
            .mapError(js.JavaScriptException)
            .orDie,
          equalTo(byteList.map(Chunk(_)))
        )
      }
    },

  )
})
