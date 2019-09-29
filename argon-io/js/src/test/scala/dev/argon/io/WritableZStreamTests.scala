package dev.argon.io

import zio._
import zio.test._
import zio.test.Assertion._

import scalajs.js
import scalajs.js.JSConverters._

object WritableZStreamTests extends DefaultRunnableSpec({
  suite("WritableZStream")(
    testM("Single Chunk") {
      checkM(Gen.listOf(Gen.anyByte)) { byteList =>
        assertM(
          WritableZStream { writable =>
            IO.effectAsync { register =>
              writable.write(NodeBuffer.from(byteList.toArray.toJSArray.map(_.toInt)), "utf8", _ => {
                register(IO.succeed(()))
              })
            }
          }
            .runCollect
            .mapError(js.JavaScriptException)
            .orDie,
          equalTo(List(Chunk.fromIterable(byteList)))
        )
      }
    },
  )
})
