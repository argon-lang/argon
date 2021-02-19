package dev.argon.platform

import dev.argon.io.fileio.{FileIO, ZipRead}
import zio.ZLayer
import zio.test._
import zio.duration._
import zio.test.environment._

abstract class PlatformRunnableSpec extends RunnableSpec[TestEnvironment with FileIO with ZipRead, Any] {
  override def aspects: List[TestAspect[Nothing, Environment, Nothing, Any]] =
    List(TestAspect.timeoutWarning(60.seconds))

  override def runner: TestRunner[Environment, Any] = {
    TestRunner(TestExecutor.default[Environment, Any](testEnvironment >>> (ZLayer.identity[TestEnvironment] ++ FileIOPlatform.live ++ ZipReadPlatform.live)))
  }
}
