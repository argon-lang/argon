package dev.argon.backend.js

import zio.test._
import zio.test.Assertion._

object JSModuleExtractorTests extends DefaultRunnableSpec {

  override def spec: ZSpec[Environment, Failure] =
    suite("JSModuleExtractor")(
      testM("Math Module")(
        assertM(JSModuleExtractorFactory.make.flatMap { _.exportedFunctions(mathModule) })(
          hasSize[(String, String)](equalTo(2)) &&
            hasKey("add") &&
            hasKey("sub")
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
