package dev.argon.platform

import zio.*
import zio.test.*

abstract class PlatformSpec[R: EnvironmentTag, E] extends ZIOSpec[R] {
  type Error = E

  type FullTestEnvironment = Environment & TestEnvironment & TestSystem

  def appBootstrapLayer: ZLayer[Any, E, R]

  final override def bootstrap: ZLayer[Any, Any, FullTestEnvironment] =
    ZLayer.succeedEnvironment(ZEnvironment[Clock, Console, System, Random](
      Clock.ClockLive,
      Console.ConsoleLive,
      NodeSystemService(),
      Random.RandomLive,
    )) >>>
      Annotations.live +!+
      Live.default +!+
      Sized.live(100) +!+
      ((Live.default ++ Annotations.live) >>> TestClock.default) +!+
      TestConfig.live(100, 100, 200, 1000) +!+
      ((Live.default ++ Annotations.live) >>> TestConsole.debug) +!+
      TestRandom.deterministic +!+
      TestSystem.default +!+
      appBootstrapLayer

  final override def spec: Spec[Environment & Scope, Any] =
    testSpec.provideSome[Scope](bootstrap)

  def testSpec: Spec[FullTestEnvironment & Scope, E]
}
