package dev.argon.stream

import cats._
import zio._
import zio.stream.ZStream
import zio.interop.catz.monadErrorInstance
import zio.test._
import zio.test.Assertion._
import zio.test.mock._

object ArStreamTests extends DefaultRunnableSpec({
  import SampleValues._
  import StreamChecker._

  suite("ArStreamTests")(
    testM("A stream from a ZStream matches") {
      assertM(runStream(ArStream.fromZStream(ZStream.fromIterable(sampleValues))), equalTo(sampleValues))
    },
    testM("Handle identity transformation (from ZStream)") {
      assertM(runStream(
        ArStream.fromZStream(ZStream.fromIterable(sampleValues)).transformWith(StreamTransformation.identity[ZIO, Any, Nothing, Int, Unit])
      ), equalTo(sampleValues))
    },
    testM("Handle flattenVector transformation (from ZStream)") {
      assertM(runStream(
        ArStream.fromZStream(ZStream.fromIterable(Vector(sampleValues))).transformWith(StreamTransformation.flattenVector[ZIO, Any, Nothing, Int, Unit])
      ), equalTo(sampleValues))
    },
    testM("A stream from a Vector matches") {
      assertM(runStream(
        ArStream.fromVector[ZIO, Any, Nothing, Int](sampleValues)
      ), equalTo(sampleValues))
    },
    testM("Handle identity transformation (from Vector)") {
      assertM(runStream(
        ArStream.fromVector[ZIO, Any, Nothing, Int](sampleValues).transformWith(StreamTransformation.identity[ZIO, Any, Nothing, Int, Unit])
      ), equalTo(sampleValues))
    },
    testM("Handle flattenVector transformation (from Vector)") {
      assertM(runStream(
        ArStream.fromVector[ZIO, Any, Nothing, Vector[Int]](Vector(sampleValues)).transformWith(StreamTransformation.flattenVector[ZIO, Any, Nothing, Int, Unit])
      ), equalTo(sampleValues))
    },
    testM("map") {
      assertM(runStream(
        ArStream.fromVector[ZIO, Any, Nothing, Int](sampleValues).map { _ + 1 }
      ), equalTo(sampleValues.map { _ + 1 }))
    },
    testM("flatMap") {
      assertM(runStream(
        ArStream.fromVector[ZIO, Any, Nothing, Vector[Int]](Vector(sampleValues)).flatMap(ArStream.fromVector[ZIO, Any, Nothing, Int])
      ), equalTo(sampleValues))
    },
  )

})
