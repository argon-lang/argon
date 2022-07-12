package dev.argon.plugins.js

import dev.argon.grammar.Grammar
import dev.argon.grammar.Grammar.Operators.*
import dev.argon.plugins.js.estree.*
import dev.argon.util.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

import scala.language.postfixOps

object ArrayTests extends ZIOSpecDefault with GenerateTestsHelper {

  override def spec: Spec[Environment & Scope, Any] =
    suite("Array AST encoding")(
      test("Empty Literal") {
        assertZIO(generate(ArrayExpression(
          elements = Seq(),
        )))(equalTo("[]"))
      },
      test("Properties") {
        assertZIO(generate(ArrayExpression(
          elements = Seq(
            Nullable(Literal(value = Nullable("A"))),
            Nullable(Literal(value = Nullable("B"))),
          ),
        )))(equalTo("[\"A\", \"B\"]"))
      },
    )

}
