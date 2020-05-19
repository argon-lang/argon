package dev.argon.backend.js

import zio.IO
import zio.test.Assertion.Render.param
import zio.test._
import zio.test.Assertion._

object JSModuleExtractorTests extends DefaultRunnableSpec {

  private def hasKey[K, V](key: K)(valueAssert: Assertion[V]): Assertion[Map[K, V]] =
    Assertion.assertionM("hasKey")(param(key))(actual => actual.get(key) match {
      case Some(value) => valueAssert.test(value)
      case None => IO.succeed(false)
    })

  override def spec: ZSpec[Environment, Failure] =
    suite("JSModuleExtractor")(
      testM("Math Module")(
        assertM(JSModuleExtractorFactory.make.flatMap { _.exportedFunctions(mathModule) })(
          hasSize[(String, String)](equalTo(2)) &&
            hasKey("add")(anything) &&
            hasKey("sub")(anything)
        )
      )
    )

  private val mathModule =
    """
      |
      |export const five = 5;
      |
      |export function add(x, y) { return x + y; }
      |
      |export function sub(x, y) { return x - y; }
      |
      |""".stripMargin

}
