package dev.argon.build.testrunner.js

import java.io.IOException

import dev.argon.io.Path
import dev.argon.build.testrunner._
import cats._
import cats.data.{NonEmptyList, NonEmptyVector}
import cats.implicits._
import zio._
import zio.interop.catz._
import dev.argon.compiler.backend.{Backend, CompilationOutputText}
import dev.argon.compiler.{CompilationError, CompilerOptions, IOCompilation}
import dev.argon.backend.js.{JSBackend, JSBackendOptions, JSInjectCode}
import dev.argon.io.{FileIO, FilenameManip, JSIOException}
import dev.argon.build._

import scala.scalajs.js
import scala.scalajs.js.{JSON, |}

final class JavaScriptTestCaseRunner(references: UIO[Vector[Path]]) extends JavaScriptTestCaseRunnerBase(references) {

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Null", "org.wartremover.warts.ToString"))
  override protected def executeJS(compiledFile: String)(modules: Seq[FileInfo]): ZIO[BuildEnvironment, Throwable, String] =
    IO.effectAsync { register =>
      val stdout = new JSMemoryWritableStream()
      val sandboxConsole = new NodeConsole(stdout)

      val sandbox = js.Object.create(null).asInstanceOf[js.Object with js.Dynamic]
      sandbox.console = js.Object.create(null)

      val _ = NodeVM.createContext(sandbox)

      val _ = NodeVM.runInContext(
        """
          |(function(outerConsole) {
          |  for(let m in outerConsole) {
          |    console[m] = function(...args) { return outerConsole[m](...args); };
          |  }
          |})
          |""".stripMargin,
        sandbox
      ).asInstanceOf[js.Dynamic](sandboxConsole)

      val linker: js.Function2[String, NodeVM.SourceTextModule, js.Promise[NodeVM.SourceTextModule]] = (specifier, referencingModule) => {
        val fileInfo = modules.find { _.name === specifier }
          .getOrElse { throw js.JavaScriptException(js.Error("Unknown module \"" + specifier + "\"")) }

        js.Promise.resolve[NodeVM.SourceTextModule](
          new NodeVM.SourceTextModule(fileInfo.content, NodeVMUtil.SourceTextModuleOptions(referencingModule.context))
        )
      }

      val mainModule = new NodeVM.SourceTextModule(
        s"""
          |import * as arCore from "Argon.Core";
          |import * as mainModule from ${JSON.stringify(moduleName)}
          |
          |mainModule.functions["main:(Ar.Unit)->(Ar.Unit)"].value(arCore.unitValue)
          |""".stripMargin,
        NodeVMUtil.SourceTextModuleOptions(sandbox)
      )

      val _ =
        mainModule
          .link(linker)
          .`then`[js.Any] { _ =>
            mainModule.instantiate()
            mainModule.evaluate()
          }
          .`then`[Unit](
              onFulfilled = _ => register(IO.succeed(stdout.toString)),
              onRejected = (
                err => register(IO.fail(new RuntimeException(err.toString + "\nCompiled output:\n" + compiledFile + "\n")))
              ) : js.Function1[Any, Unit | js.Thenable[Unit]]
          )
    }



}

object JavaScriptTestCaseRunner {

  def apply(references: UIO[Vector[Path]]): JavaScriptTestCaseRunner =
    new JavaScriptTestCaseRunner(references)

}
