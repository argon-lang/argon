package dev.argon.platform

import dev.argon.io.fileio.{FileIO, FileIOLite}
import zio.test._
import zio.duration._
import zio.test.environment._

abstract class PlatformRunnableSpec extends RunnableSpec[TestEnvironment with FileIO[FilePath] with FileIOLite, Any] {
  override def aspects: List[TestAspect[Nothing, Environment, Nothing, Any]] =
    List(TestAspect.timeoutWarning(60.seconds))

  override def runner: TestRunner[Environment, Any] =
    TestRunner(TestExecutor.default[Environment, Any](
      ((liveEnvironment ++ NodeSystem.live) >>> TestEnvironment.live) ++ FileIOPlatform.live ++ FileIOLitePlatform.live))
}
