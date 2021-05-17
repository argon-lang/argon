package dev.argon.build.testrunner.js

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@js.native
@JSImport("vm", JSImport.Namespace)
object NodeVM extends js.Any {

  def createContext(@unused obj: js.Any): js.Any = js.native
  def runInContext(@unused code: String, @unused context: js.Any): js.Any = js.native

  @js.native
  trait SourceTextModuleOptions extends js.Any {
    val context: js.Any = js.native
  }

  @js.native
  class SourceTextModule(@unused code: String, @unused options: SourceTextModuleOptions) extends js.Object {
    def link(@unused linker: js.Function2[String, SourceTextModule, js.Promise[SourceTextModule]]): js.Promise[Unit] = js.native
    def evaluate(): js.Promise[js.Any] = js.native
    val context: js.Any = js.native
  }



}

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
object NodeVMUtil {
  import NodeVM.SourceTextModuleOptions

  object SourceTextModuleOptions {
    def apply(context: js.Any): SourceTextModuleOptions =
      js.Dynamic.literal(context = context).asInstanceOf[SourceTextModuleOptions]
  }

}
