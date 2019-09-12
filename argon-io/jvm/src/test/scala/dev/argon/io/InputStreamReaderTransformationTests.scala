package dev.argon.io

import java.io.{ByteArrayInputStream, InputStream}

import dev.argon.stream.ArStream
import zio._
import zio.interop.catz._
import zio.blocking.Blocking
import zio.stream.ZStream
import zio.test._
import zio.test.Assertion._

import scala.collection.mutable.ArrayBuffer

object InputStreamReaderTransformationTests extends DefaultRunnableSpec({
  import StreamCommon._

  suite("InputStreamReaderTransformationTests")(
    testM("transform stream using single byte read") {
      assertM(InputStreamReaderTransformation(sampleZStream)(usingSingleByteRead).orDie, equalTo(streamContent))
    },
    testM("transform stream using buffer reads") {
      assertM(InputStreamReaderTransformation(sampleZStream)(usingBufferReader).orDie, equalTo(streamContent))
    },
    testM("handle extra single byte reads") {
      assertM(InputStreamReaderTransformation(sampleZStream)(usingSingleByteReadExtra).orDie, equalTo(retryCount))
    },
    testM("handle extra buffer reads") {
      assertM(InputStreamReaderTransformation(sampleZStream)(usingBufferReaderExtra).orDie, equalTo(retryCount))
    },
    testM("handle empty single byte reads") {
      assertM(InputStreamReaderTransformation(emptyZStream)(usingSingleByteRead).orDie, equalTo(Seq[Byte]()))
    },
    testM("handle empty buffer reads") {
      assertM(InputStreamReaderTransformation(emptyZStream)(usingBufferReader).orDie, equalTo(Seq[Byte]()))
    },
    testM("handle no reads") {
      assertM(InputStreamReaderTransformation(emptyZStream)(noReads).orDie, equalTo(()))
    },
  )


})
