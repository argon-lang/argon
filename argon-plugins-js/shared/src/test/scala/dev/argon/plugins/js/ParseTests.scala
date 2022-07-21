package dev.argon.plugins.js

import dev.argon.grammar.Grammar
import dev.argon.grammar.Grammar.Operators.*
import dev.argon.plugins.js.estree.*
import dev.argon.util.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

import scala.language.postfixOps

object ParseTests extends ZIOSpecDefault {

  private def parseGen(code: String): IO[Any, String] =
    ZIO.scoped(JSContext.make.flatMap { context =>
      context.parse("test.js", code).flatMap(context.generate)
    })


  override def spec: Spec[Environment & Scope, Any] =
    suite("Parse and Generate")(
      test("String") {
        assertZIO(parseGen("\"a\"")
        )(equalTo("\"a\";\n"))
      },
      test("Boolean") {
        assertZIO(parseGen("true")
        )(equalTo("true;\n"))
      },
      test("BigInt") {
        assertZIO(parseGen("55n")
        )(equalTo("55n;\n"))
      },
    )

}
