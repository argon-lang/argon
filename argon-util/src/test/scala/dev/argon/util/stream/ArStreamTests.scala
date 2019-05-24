package dev.argon.util.stream

import org.scalatest.{FlatSpec, Matchers}
import cats._
import cats.implicits._
import scalaz.zio._
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
    unsafeRun(checkStream(sampleValues, ArStream.fromZStream(ZStream.fromIterable(sampleValues)).transformWith(StreamTransformation.identity[ZIO, Any, Int, Int, Unit])))
  }

  it should "handle flattenVector transformation" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromZStream(ZStream.fromIterable(Vector(sampleValues))).transformWith(StreamTransformation.flattenVector[ZIO, Any, Int, Int, Unit])))
  }

  it should "handle toVector transformation" in {
    unsafeRun(ArStream.fromZStream(ZStream.fromIterable(sampleValues)).foldLeft(StreamTransformation.toVector[ZIO, Any, Int, Int])) shouldEqual sampleValues
  }


  "A stream from a Vector" should "match" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromVector[ZIO, Any, Int, Int](sampleValues)))
  }

  it should "convert to ZStream" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromVector[ZIO, Any, Int, Int](sampleValues)))
  }

  it should "handle identity transformation" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromVector[ZIO, Any, Int, Int](sampleValues).transformWith(StreamTransformation.identity[ZIO, Any, Int, Int, Unit])))
  }

  it should "handle flattenVector transformation" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromVector[ZIO, Any, Int, Vector[Int]](Vector(sampleValues)).transformWith(StreamTransformation.flattenVector[ZIO, Any, Int, Int, Unit])))
  }

  it should "handle toVector transformation" in {
    unsafeRun(ArStream.fromVector[ZIO, Any, Int, Int](sampleValues).foldLeft(StreamTransformation.toVector[ZIO, Any, Int, Int])) shouldEqual sampleValues
  }

  it should "map" in {
    unsafeRun(checkStream(sampleValues.map { _ + 1 }, ArStream.fromVector[ZIO, Any, Int, Int](sampleValues).map { _ + 1 }))
  }

  it should "flatMap" in {
    unsafeRun(checkStream(sampleValues, ArStream.fromVector[ZIO, Any, Int, Vector[Int]](Vector(sampleValues)).flatMap(ArStream.fromVector[ZIO, Any, Int, Int])))
  }
}
