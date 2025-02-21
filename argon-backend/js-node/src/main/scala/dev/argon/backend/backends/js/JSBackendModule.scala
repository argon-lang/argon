package dev.argon.backend.backends.js

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import dev.argon.backend.sjs


@JSImport("@argon-lang/compiler-backend-js", JSImport.Namespace)
@js.native
object JSBackendModule extends js.Object {
  val backendFactory: sjs.BackendFactory = js.native
}

