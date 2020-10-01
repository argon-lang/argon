package dev.argon.build.testrunner

import java.io.IOException
import java.lang.Runtime.{Version => JDKVersion}

import cats.data.NonEmptyList
import dev.argon.build.testrunner.TestCaseRunnerCompilePhase.{TestCaseInputSource, TestCaseOtherRes, TestCompileResource}
import dev.argon.compiler._
import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.io.JarFileReader
import zio._

private[testrunner] object TestCaseRunnerCompilePhasePlatformSpecific {
  trait ResourceReaderService[I <: ResourceIndicator] {
    protected implicit val resIndicatorTag: Tag[I]
    protected val outerReaderEnv: ResourceReader[I]

    def getJarReader(id: TestCompileResource[I], jdkVersion: JDKVersion): Managed[CompilationError, JarFileReader[Any, CompilationError]] =
      id match {
        case TestCaseInputSource(_) =>
          Managed.fail(DiagnosticError.ResourceIOError(
            DiagnosticSource.ThrownException(new IOException("Invalid file format. Not a zip file."))
          ))
        case TestCaseOtherRes(id) => outerReaderEnv.get.getJarReader(id, jdkVersion)
      }
  }
}
