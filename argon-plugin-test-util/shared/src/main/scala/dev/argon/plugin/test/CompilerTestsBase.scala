package dev.argon.plugin.test

import zio.*
import zio.test.*

abstract class CompilerTestsBase extends ZIOSpecDefault {

  def suiteName: String

  protected[test] def createTest(testCase: TestCase): Spec[TestEnvironment with Scope, Any]


  private def createSpec(groupName: String)(testCases: TestCases): Spec[TestEnvironment with Scope, Any] =
    testCases match
      case TestCases.Group(tests) =>
        val suites = tests.toSeq.map { case (name, subCases) => createSpec(name)(subCases) }
        suite(groupName)(suites*)

      case TestCases.Test(test) =>
        createTest(test)
    end match

  override def spec: Spec[TestEnvironment with Scope, Any] =
    Spec.scoped[TestEnvironment with Scope](
      TestCases.testCases
        .mapErrorCause { cause => Cause.fail(TestFailure.Runtime(cause)) }
        .map(createSpec(suiteName))
    )


}
