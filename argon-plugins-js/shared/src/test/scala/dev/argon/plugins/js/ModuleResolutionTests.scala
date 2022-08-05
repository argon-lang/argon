package dev.argon.plugins.js

import dev.argon.grammar.Grammar
import dev.argon.grammar.Grammar.Operators.*
import dev.argon.plugins.js.estree.*
import dev.argon.util.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

import zio.json.ast.Json

import scala.language.postfixOps

object ModuleResolutionTests extends ZIOSpecDefault with GenerateTestsHelper {

  override def spec: Spec[Environment & Scope, Any] =
    suite("Module resolution")(
      suite("packageExportsResolve")(
        test("subpath . with object") {
          val exports = Json.Obj(
            "." -> Json.Str("./index.js"),
          )

          val moduleResolution = ModuleResolution(Map.empty)

          val resolved = moduleResolution.packageExportsResolve("/test/mypackage", ".", exports, moduleResolution.defaultConditions)

          assertTrue("/test/mypackage/index.js" == resolved)
        }
      ),
      suite("packageTargetResolve")(
        test("empty subpath") {
          val moduleResolution = ModuleResolution(Map.empty)

          val resolved = moduleResolution.packageTargetResolve("/test/mypackage", Json.Str("./index.js"), "", false, false, moduleResolution.defaultConditions)

          assertTrue("/test/mypackage/index.js" == resolved.get)
        }
      ),
    )

}
