package dev.argon.io

import zio._
import zio.test._
import zio.test.Assertion._

import scalajs.js
import scalajs.js.JSConverters._

object WritableZStreamTests extends DefaultRunnableSpec({
  val chunk = Chunk[Byte](4, 50)

  suite("WritableZStream")(
    testM("Single Chunk") {
      assertM(
        WritableZStream { writable =>
          IO.effectAsync { register =>
            writable.write(NodeBuffer.from(chunk.toArray.toJSArray.map(_.toInt)), "utf8", _ => {
              register(IO.succeed(()))
            })
          }
        }
          .runCollect
          .mapError(js.JavaScriptException)
          .orDie,
        equalTo(List(Chunk.fromArray(chunk.toArray)))
      )
    },
  )
})
