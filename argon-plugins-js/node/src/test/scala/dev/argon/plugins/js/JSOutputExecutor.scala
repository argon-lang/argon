package dev.argon.plugins.js

import scala.util.Try
import scala.scalajs.js
import scala.scalajs.js.{JSON, UndefOr}
import scala.collection.mutable
import scala.collection.mutable.StringBuilder
import zio.*

object JSOutputExecutor {
  def run(fileSystem: Map[String, String]): Task[String] =
    ZIO.suspend {
      val moduleResolution = ModuleResolution(fileSystem)


      val stdout = new NodeWritableStream {
        private val sb: StringBuilder = StringBuilder()
        override protected def _write(chunk: js.Any, encoding: String, callback: js.Function1[UndefOr[js.Any], Unit]): Unit =
          sb.append(chunk)
          callback(js.undefined)
        end _write

        override protected def _writev(chunks: js.Array[NodeWritableStream.Chunk], callback: js.Function1[UndefOr[js.Any], Unit]): Unit =
          chunks.foreach { chunk => sb.append(chunk.chunk) }
          callback(js.undefined)
        end _writev

        override def toString(): String = sb.result()
      }
      val sandboxConsole = new NodeConsole(stdout)

      val sandbox = js.Object.create(null.asInstanceOf[js.Object]).asInstanceOf[js.Object & js.Dynamic]
      sandbox.console = js.Object.create(null.asInstanceOf[js.Object])

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

      val modules = mutable.Map[String, NodeVM.SourceTextModule]()
      val modulePaths = mutable.Map[NodeVM.SourceTextModule, String]()

      def addModule(path: String, module: NodeVM.SourceTextModule): Unit =
        modules(path) = module
        modulePaths(module) = path
      end addModule


      val linker: js.Function2[String, NodeVM.SourceTextModule, js.Promise[NodeVM.SourceTextModule]] = (specifier, referencingModule) => {
        (Try {
          val resolvedSpecifier = moduleResolution.resolveSpecifier(specifier, modulePaths(referencingModule))
          modules(resolvedSpecifier)
        }).fold(js.Promise.reject, js.Promise.resolve)
      }

      val mainModule = new NodeVM.SourceTextModule(
        """
          |import * as argonRuntime from "@argon-lang/runtime";
          |import { main$p$z$r$z } from "@tubes/Test";
          |
          |await argonRuntime.trampoline.resolve(main$p$z$r$z([]));
          |""".stripMargin,
        new NodeVM.SourceTextModuleOptions {
          override val context: js.Any = sandbox
        }
      )

      addModule("/argon/main.js", mainModule)

      fileSystem
        .iterator
        .filter { case (path, _) => path.endsWith(".js") }
        .foreach { case (path, content) =>
          val options = new NodeVM.SourceTextModuleOptions {
            override val context: js.Any = sandbox
          }

          addModule(path, new NodeVM.SourceTextModule(content, options))
        }


      for {
        _ <- ZIO.fromPromiseJS(mainModule.link(linker))
        _ <- ZIO.fromPromiseJS(mainModule.evaluate())
        output <- ZIO.attempt { stdout.toString() }
      } yield output
    }

}
