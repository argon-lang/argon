package dev.argon.build.testrunner.js

import java.io.{FileNotFoundException, IOException}

import dev.argon.build.testrunner._
import cats.implicits._
import zio._
import zio.interop.catz.core._
import dev.argon.compiler.CompilationError
import dev.argon.backend.js.{JSBackend, JSOutputOptionID}
import dev.argon.io.fileio.{FileIO, ZipRead}
import dev.argon.build._
import dev.argon.options.FileList
import dev.argon.util.MaybeBlocking
import dev.argon.io.FileNameUtil

abstract class JavaScriptTestCaseRunnerBase extends TestCaseRunnerExecutionPhase[FileIO with ZipRead with MaybeBlocking] {

  override val name: String = "JavaScript Execution"
  override protected val backend: JSBackend



  override protected def getProgramOutput(buildResult: BuildResult.Aux[backend.type]): ZIO[FileIO, CompilationError, String] =
    for {
      compiledFile <- buildResult.backendOutput.get(JSOutputOptionID.JSModule).asString
      output <- runJSOutput(references)(compiledFile).orDie
    } yield output

  private def runJSOutput(files: FileList)(compiledFile: String): RIO[FileIO, String] = for {
    referenceLibs <- files.files.traverse { libFileName =>
      for {
        libDir <- IO.fromEither(FileNameUtil.getParentDirectory(libFileName).toRight { new FileNotFoundException() })
        libName = FileNameUtil.getBaseNameWithoutExtension(libFileName)
        jsLibPath = FileNameUtil.combine(libDir, FileNameUtil.combine("js", libName + ".js"))
        content <- ZIO.accessM[FileIO] { _.get.readAllText(jsLibPath) }
      } yield FileInfo(libName, content)
    }

    modules = (referenceLibs :+ FileInfo(moduleName, compiledFile))

    output <- executeJS(compiledFile)(modules).orDie
  } yield output

  protected def executeJS(compiledFile: String)(modules: Seq[FileInfo]): Task[String]

}

