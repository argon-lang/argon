package dev.argon.build.testrunner.js


import dev.argon.backend.js.JSBackend
import zio._
import dev.argon.options.FileList

import scala.scalajs.js
import scala.scalajs.js.JSON

final class JavaScriptNodeVMTestCaseRunner(protected val backend: JSBackend, protected val references: FileList) extends JavaScriptTestCaseRunnerBase {

  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf", "scalafix:DisableSyntax.null", "scalafix:Disable.toString", "dev.argon.warts.ZioEffect"))
  override protected def executeJS(compiledFile: String)(modules: Seq[FileInfo]): Task[String] =
    IO.effectSuspend {
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

      val linker: js.Function2[String, NodeVM.SourceTextModule, js.Promise[NodeVM.SourceTextModule]] = (specifier, _) => {
        val module = vmModules.getOrElse(specifier, throw js.JavaScriptException(js.Error("Unknown module \"" + specifier + "\"")))
        js.Promise.resolve[NodeVM.SourceTextModule](module)
      }

      val mainModule = new NodeVM.SourceTextModule(
        s"""
           |import arCore, { unitValue } from "Argon.Core";
           |import mainModule from ${JSON.stringify(moduleName)};
           |
           |const unitType = { type: "class", arClass: arCore.globalClass(["Ar"], "Unit", { parameterTypes: [] }), arguments: [] };
           |mainModule.globalFunction([], "main", { parameterTypes: [unitType], resultType: unitType }).invoke(unitValue);
           |""".stripMargin,
        NodeVMUtil.SourceTextModuleOptions(sandbox)
      )

      for {
        _ <- ZIO.fromPromiseJS(mainModule.link(linker))
        _ <- IO.effect {
          mainModule.evaluate()
        }
        output <- IO.effect { stdout.toString }
      } yield output
    }



}

