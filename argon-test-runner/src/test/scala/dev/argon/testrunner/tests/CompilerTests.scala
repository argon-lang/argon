package dev.argon.testrunner.tests

import dev.argon.testrunner.GroupedTestCase
import zio.*
import zio.test.*
import zio.test.Assertion.*
import scala.jdk.CollectionConverters.*


object CompilerTests extends ZIOSpecDefault {

  private val testSuiteCreators: Seq[() => CompilerTestSuiteBase] = Seq(
    JVMCompilerCLITestSuite.apply,
    JVMCompilerJSBackendTestSuite.apply,
    NodeCompilerJSBackendTestSuite.apply,
  )

  override def spec: Spec[TestEnvironment & Scope, Any] =
    suite("Compiler Tests")(
      testSuiteCreators.map(loadTestSuite)*
    ) @@ TestAspect.parallelN(java.lang.Runtime.getRuntime.availableProcessors())

  private def loadTestSuite(suiteCreator: () => CompilerTestSuiteBase): Spec[TestEnvironment & Scope, Any] =
    Spec.scoped(
      for
        s <- ZIO.fromAutoCloseable(ZIO.attempt(suiteCreator()).mapError(TestFailure.die))
        testCases <- ZIO.attempt(s.loadTestCases()).mapError(TestFailure.die)
      yield suite(s.getSuiteName)(
        testCaseSuites(s, prefixLength = 0)(testCases.asScala.toSeq)*
      )
    )

  private def testCaseSuites(s: CompilerTestSuiteBase, prefixLength: Int)(testCases: Seq[GroupedTestCase]): Seq[Spec[TestEnvironment & Scope, Any]] =
    testCases
      .groupBy { tc => tc.getGroup.asScala.view.drop(prefixLength).headOption }
      .flatMap { (groupName, groupTests) =>
        groupName match {
          case Some(groupName) =>
            Seq(
              suite(groupName)(testCaseSuites(s, prefixLength = prefixLength + 1)(groupTests)*),
            )

          case None => groupTests.map(createTest(s))
        }
      }
      .toSeq
      
  private def createTest(s: CompilerTestSuiteBase)(tc: GroupedTestCase): Spec[TestEnvironment & Scope, Any] =
    test(tc.getBaseName) {
      for
        testResult <- ZIO.attempt(s.runTestCase(tc))
      yield assertTrue(tc.getExpectedResult.matchedBy(testResult))
    }
    
    
    
}
