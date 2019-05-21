package dev.argon.util.stream

import org.scalatest.{FlatSpec, Matchers}
import cats._
import cats.implicits._
import scalaz.zio.{DefaultRuntime, IO}
import scalaz.zio.stream.ZStream
import scalaz.zio.interop.catz._

class ArStreamTests extends FlatSpec with Matchers with DefaultRuntime with SampleValues with StreamChecker {

  "A stream from a ZStream" should "match" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromZStream(ZStream.fromIterable(sampleValues))))
  }

  it should "convert to ZStream" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromZStream(ZStream.fromIterable(sampleValues))))
  }

  it should "handle identity transformation" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromZStream(ZStream.fromIterable(sampleValues)).transformWith(StreamTransformation.identity[IO, Int, Int, Unit])))
  }

  it should "handle flattenVector transformation" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromZStream(ZStream.fromIterable(Vector(sampleValues))).transformWith(StreamTransformation.flattenVector[IO, Int, Int, Unit])))
  }

  it should "handle toVector transformation" in {
    unsafeRun(ArStream.fromZStream(ZStream.fromIterable(sampleValues)).foldLeft(StreamTransformation.toVector[IO, Int, Int])) shouldEqual sampleValues
  }


  "A stream from a Vector" should "match" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromVector[IO, Int, Int](sampleValues)))
  }

  it should "convert to ZStream" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromVector[IO, Int, Int](sampleValues)))
  }

  it should "handle identity transformation" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromVector[IO, Int, Int](sampleValues).transformWith(StreamTransformation.identity[IO, Int, Int, Unit])))
  }

  it should "handle flattenVector transformation" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromVector[IO, Int, Vector[Int]](Vector(sampleValues)).transformWith(StreamTransformation.flattenVector[IO, Int, Int, Unit])))
  }

  it should "handle toVector transformation" in {
    unsafeRun(ArStream.fromVector[IO, Int, Int](sampleValues).foldLeft(StreamTransformation.toVector[IO, Int, Int])) shouldEqual sampleValues
  }
}
