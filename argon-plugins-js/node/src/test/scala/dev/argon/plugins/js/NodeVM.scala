package dev.argon.plugins.js


import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("node:vm", JSImport.Namespace)
object NodeVM extends js.Any {

  def createContext(obj: js.Any): js.Any = js.native

  def runInContext(code: String, context: js.Any): js.Any = js.native

  trait SourceTextModuleOptions extends js.Object {
    val context: js.Any
  }

  @js.native
  class SourceTextModule(code: String, options: SourceTextModuleOptions) extends js.Object {
    def link(linker: js.Function2[String, SourceTextModule, js.Promise[SourceTextModule]]): js.Promise[Unit] = js.native

    def evaluate(): js.Promise[js.Any] = js.native

    val context: js.Any = js.native
  }

}
