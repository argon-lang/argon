package dev.argon.build

import cats._
import cats.implicits._
import dev.argon.build.testrunner._
import dev.argon.io.Path
import zio._
import zio.interop.catz._
import zio.test._
import zio.test.Assertion._
import zio.test.mock._

import CompilerTestSuiteFactory._

object CompilerTests extends DefaultRunnableSpec(
  suite("Compiler Tests")(
    runners.map { runner =>
      suite(runner.name)(
        createSuites(runner, testCases): _*
      )
    }: _*
  )
)
