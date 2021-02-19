package dev.argon.util

import zio._
import zio.test.environment.TestEnvironment
import zio.test._
import zio.test.Assertion._
import zio.stream._

object StreamMemoTests extends DefaultRunnableSpec {

  private val simpleStreamContent = Chunk(5, 7, 1111, 58)
  private val extraStreamContent = Chunk(77, 48, 97)

  private def runMemo(stream: Stream[Nothing, Int]): UIO[Chunk[Int]] =
    StreamMemo.make(stream).use { memoStream =>
      memoStream.runCollect
    }

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("StreamMemo")(
      testM("Simple stream")(
        assertM(runMemo(Stream.fromChunk(simpleStreamContent)))(equalTo(simpleStreamContent))
      ),
      testM("Extra stream")(
        assertM(runMemo(Stream.fromChunk(simpleStreamContent) ++ Stream.fromChunk(extraStreamContent)))(equalTo(simpleStreamContent ++ extraStreamContent))
      ),
    )
}
