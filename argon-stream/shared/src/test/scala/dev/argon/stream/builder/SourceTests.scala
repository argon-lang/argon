package dev.argon.stream.builder

import zio.{IO, ZIO}
import zio.test.environment.TestEnvironment
import zio.test._
import zio.test.Assertion._

object SourceTests extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Source tests")(
      testM("Handles die")(
        assertM(
          new Source[Any, Nothing, Unit] {
            override def foreach(f: Unit => ZIO[Any, Nothing, Unit]): ZIO[Any, Nothing, Unit] =
              IO.die(new RuntimeException)
          }.toZStream.runDrain.run
        )(dies(anything))
      )
    )
}
