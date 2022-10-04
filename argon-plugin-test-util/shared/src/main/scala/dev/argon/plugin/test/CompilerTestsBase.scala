package dev.argon.plugin.test

import zio.*
import zio.test.*

abstract class CompilerTestsBase extends ZIOSpecDefault {

  def suiteName: String

  protected[test] def createTest(testCase: TestCase): Spec[TestEnvironment & Scope, Any]


  private def createSpec(groupName: String)(testCases: TestCases): Spec[TestEnvironment & Scope, Any] =
    testCases match
      case TestCases.Group(tests) =>
        val suites = tests.toSeq.map { case (name, subCases) => createSpec(name)(subCases) }
        suite(groupName)(suites*)

      case TestCases.Test(test) =>
        createTest(test) @@ TestAspect.timeoutWarning(1.minute)
    end match

  override def spec: Spec[TestEnvironment & Scope, Any] =
    Spec.scoped[TestEnvironment & Scope](
      TestCases.testCases
        .mapErrorCause { cause => Cause.fail(TestFailure.Runtime(cause)) }
        .map(createSpec(suiteName))
    )


}
