package dev.argon.backend.backends.js

import scala.scalajs.js
import dev.argon.util.async.AsyncIterableTools.AsyncIterable
import scala.scalajs.js.annotation.JSImport
import dev.argon.backend.sjs

trait CodegenInput extends js.Object {
  val tubeMapping: js.Array[TubeMapping]
  val tubeInput: TubeInput
  def getExterns(): AsyncIterable[ExternsInfo]
}

sealed trait TubeInput extends js.Object {
  val `type`: String
}

object TubeInput {
  trait Ir extends TubeInput {
    override val `type`: "ir"
    def entries(): AsyncIterable[dev.argon.vm.sjs.TubeFileEntry]
  }
}

trait ExternsInfo extends js.Object {
  val sourceCode: String
  val sourceFile: String
}

trait ModuleCodegenResult extends js.Object {
  val moduleFilePath: js.Array[String]
  val sourceCode: String
}

trait TubeMapping extends js.Object {
  val tubeName: dev.argon.vm.sjs.TubeName
  val packageName: String
}

@JSImport("@argon-lang/js-compiler-backend", JSImport.Namespace)
@js.native
object JSBackendModule extends js.Object {
  def codegen(input: CodegenInput): AsyncIterable[ModuleCodegenResult] = js.native
  
  val backendFactory: sjs.BackendFactory = js.native
}

