package dev.argon.stream

import cats._
import cats.implicits._
import zio._
import zio.stream.ZStream
import zio.interop.catz.monadErrorInstance
import SampleValues._
import StreamChecker._
import zio.test._
import zio.test.Assertion._
import zio.test.mock._

object StreamTransformationTests extends DefaultRunnableSpec(
  suite("StreamTransformationTests")(
    testM("into") {
      assertM(runStream(
        ArStream.fromVector[ZIO, Any, Nothing, Int](sampleValues).transformWith(
          StreamTransformation.identity[ZIO, Any, Nothing, Int, Unit].into(StreamTransformation.identity[ZIO, Any, Nothing, Int, Unit])
        )
      ), equalTo(sampleValues))
    },
    testM("buffer") {
      assertM(runStream(
        ArStream.fromVector[ZIO, Any, Nothing, Int](sampleValues).transformWith(
          StreamTransformation.identity[ZIO, Any, Nothing, Int, Unit].buffer(10)
        )
      ), equalTo(sampleValues))
    },
    testM("buffer twice") {
      assertM(runStream(
        ArStream.fromVector[ZIO, Any, Nothing, Int](sampleValues).transformWith(
          StreamTransformation.identity[ZIO, Any, Nothing, Int, Unit]
            .buffer(10)
            .into(StreamTransformation.identity[ZIO, Any, Nothing, Int, Unit])
            .buffer(10)
        )
      ), equalTo(sampleValues))
    },
    testM("collect") {
      assertM(runStream(
        ArStream.fromVector[ZIO, Any, Nothing, Int](sampleValues).transformWith(
          StreamTransformation.identity[ZIO, Any, Nothing, Int, Unit].collect {
            case x if x =!= 100 => x + 1
          }
        )
      ), equalTo(sampleValues.filter { _ =!= 100 }.map { _ + 1 }))
    },
    testM("map") {
      assertM(runStream(
        ArStream.fromVector[ZIO, Any, Nothing, Int](sampleValues).transformWith(
          StreamTransformation.identity[ZIO, Any, Nothing, Int, Unit].map { _ + 1 }
        )
      ), equalTo(sampleValues.map { _ + 1 }))
    },
  )
)
