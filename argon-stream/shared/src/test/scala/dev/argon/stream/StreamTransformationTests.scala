package dev.argon.stream

import cats.implicits._
import zio.test._
import zio.test.Assertion._
import zio.random.Random
import zio.stream.ZStream
import StreamExtensions._
import zio.IO

object StreamTransformationTests extends DefaultRunnableSpec {
  override def spec: ZSpec[Environment, Failure] =
    suite("StreamTransformation tests")(
      testM("StreamMapper") {
        checkM(Gen.listOf[Random with Sized, Byte](Gen.anyByte)) { byteList =>
          assertM(ZStream.fromIterable(byteList).transformWith(
            new StreamMapper[Any, Nothing, Byte, Unit, Byte](b => IO.succeed((b + 1).toByte))
          ).runCollect)(equalTo(byteList.map { b => (b + 1).toByte }))
        }
      },
      testM("StreamCollector") {
        checkM(Gen.listOf[Random with Sized, Byte](Gen.anyByte)) { byteList =>
          assertM(ZStream.fromIterable(byteList).transformWith(
            new StreamCollector[Any, Nothing, Byte, Unit, Byte]({
              case b if b % 2 === 0 => IO.succeed((b / 2).toByte)
            })
          ).runCollect)(equalTo(byteList.filter(_ % 2 === 0).map(b => (b / 2).toByte)))
        }
      }
    )
}