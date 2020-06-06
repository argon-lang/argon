package dev.argon.build.testrunner.js

import java.io.IOException

import dev.argon.io.Path
import cats.implicits._
import dev.argon.backend.js.JSBackend
import zio._
import dev.argon.build._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.fileio.FileIO

import scala.scalajs.js
import scala.scalajs.js.{JSON, |}

final class JavaScriptNodeVMTestCaseRunner[I <: ResourceIndicator: Tag, P: Path : Tag](protected val backend: JSBackend, protected val references: Vector[I], pathResolver: I => UIO[P]) extends JavaScriptTestCaseRunnerBase[I, P](pathResolver) {

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

      val vmModules = modules.map { fileInfo =>
        fileInfo.name -> new NodeVM.SourceTextModule(fileInfo.content, NodeVMUtil.SourceTextModuleOptions(sandbox))
      }.toMap

      val linker: js.Function2[String, NodeVM.SourceTextModule, js.Promise[NodeVM.SourceTextModule]] = (specifier, referencingModule) => {
        val module = vmModules.getOrElse(specifier, throw js.JavaScriptException(js.Error("Unknown module \"" + specifier + "\"")))
        js.Promise.resolve[NodeVM.SourceTextModule](module)
      }

      val mainModule = new NodeVM.SourceTextModule(
        s"""
          |import arCore, { unitValue } from "Argon.Core";
          |import mainModule from ${JSON.stringify(moduleName)};
          |
          |const unitType = { type: "class", arClass: arCore.globalClass(["Ar"], "Unit", { parameterTypes: [] }) };
          |mainModule.globalFunction([], "main", { parameterTypes: [unitType], resultType: unitType }).invoke(unitValue);
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

