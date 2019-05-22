package dev.argon.util.stream

import org.scalatest.{FlatSpec, Matchers}
import cats._
import cats.implicits._
import scalaz.zio.{DefaultRuntime, IO}
import scalaz.zio.stream.ZStream
import scalaz.zio.interop.catz._

class StreamTransformationTests extends FlatSpec with Matchers with DefaultRuntime with SampleValues with StreamChecker {

  "A stream transformation" should "handle into" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromVector[IO, Int, Int](sampleValues).transformWith(
      StreamTransformation.identity[IO, Int, Int, Unit].into(StreamTransformation.identity[IO, Int, Int, Unit])
    )))
  }

  it should "handle buffer" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromVector[IO, Int, Int](sampleValues).transformWith(
      StreamTransformation.identity[IO, Int, Int, Unit].buffer(10)
    )))
  }




}
