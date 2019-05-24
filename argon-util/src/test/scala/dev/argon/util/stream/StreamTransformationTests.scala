package dev.argon.util.stream

import org.scalatest.{FlatSpec, Matchers}
import cats._
import cats.implicits._
import scalaz.zio._
import scalaz.zio.stream.ZStream
import scalaz.zio.interop.catz._

class StreamTransformationTests extends FlatSpec with Matchers with DefaultRuntime with SampleValues with StreamChecker {

  "A stream transformation" should "handle into" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromVector[ZIO, Any, Int, Int](sampleValues).transformWith(
      StreamTransformation.identity[ZIO, Any, Int, Int, Unit].into(StreamTransformation.identity[ZIO, Any, Int, Int, Unit])
    )))
  }

  it should "handle buffer" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromVector[ZIO, Any, Int, Int](sampleValues).transformWith(
      StreamTransformation.identity[ZIO, Any, Int, Int, Unit].buffer(10)
    )))
  }

  it should "handle buffer twice" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromVector[ZIO, Any, Int, Int](sampleValues).transformWith(
      StreamTransformation.identity[ZIO, Any, Int, Int, Unit]
        .buffer(10)
        .into(StreamTransformation.identity[ZIO, Any, Int, Int, Unit])
        .buffer(10)
    )))
  }

  it should "work for ZStream" in {
    unsafeRun(checkZStream(sampleValues, ZStream.fromIterable(sampleValues).transformIO(
      StreamTransformation.identity[ZIO, Any, Int, Int, Unit]
    )))
  }

  it should "handle collect" in {
    unsafeRun(checkStream(sampleValues.filter { _ =!= 100 }.map { _ + 1 }, ArStream.fromVector[ZIO, Any, Int, Int](sampleValues).transformWith(
      StreamTransformation.identity[ZIO, Any, Int, Int, Unit].collect {
        case x if x =!= 100 => x + 1
      }
    )))
  }

  it should "handle map" in {
    unsafeRun(checkStream(sampleValues.map { _ + 1 }, ArStream.fromVector[ZIO, Any, Int, Int](sampleValues).transformWith(
      StreamTransformation.identity[ZIO, Any, Int, Int, Unit].map { _ + 1 }
    )))
  }



}
