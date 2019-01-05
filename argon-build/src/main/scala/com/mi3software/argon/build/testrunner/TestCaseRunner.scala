package com.mi3software.argon.build.testrunner

import com.mi3software.argon.compiler.CompilationError
import com.mi3software.argon.util.{CatsInstances, FileID, FileSpec}
import scalaz._
import Scalaz._
import com.mi3software.argon.parser.SyntaxError
import fs2._
import com.mi3software.argon.parser.impl.ParseHandler
import shims._

trait TestCaseRunner {
  def runTest[F[_]: cats.effect.Sync](testCase: TestCase): F[TestCaseResult]
}
