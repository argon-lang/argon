package dev.argon.build.testrunner.js

import java.io.IOException

import dev.argon.io.Path
import cats.implicits._
import zio._
import dev.argon.build._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.fileio.FileIO

import scala.scalajs.js
import scala.scalajs.js.{JSON, |}

final class JavaScriptNodeVMTestCaseRunner(protected val references: Vector[ResourceIndicator]) extends JavaScriptTestCaseRunnerBase {

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf", "org.wartremover.warts.Null", "org.wartremover.warts.ToString", "dev.argon.warts.ZioEffect"))
  override protected def executeJS(compiledFile: String)(modules: Seq[FileInfo]): Task[String] =
    IO.effectAsync { register =>
      val stdout = new MemoryWritableStream()
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
          |import * as mainModule from ${JSON.stringify(moduleName)};
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

