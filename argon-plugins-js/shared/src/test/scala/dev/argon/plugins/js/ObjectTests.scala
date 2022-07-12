package dev.argon.plugins.js

import dev.argon.grammar.Grammar
import dev.argon.grammar.Grammar.Operators.*
import dev.argon.plugins.js.estree.*
import dev.argon.util.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

import scala.language.postfixOps

object ObjectTests extends ZIOSpecDefault with GenerateTestsHelper {

  override def spec: Spec[Environment & Scope, Any] =
    suite("Object AST encoding")(
      test("Empty Literal") {
        assertZIO(generate(ObjectExpression(
          properties = Seq(),
        )))(equalTo("{}"))
      },
      test("Properties") {
        assertZIO(generate(ObjectExpression(
          properties = Seq(
            Property(
              key = Identifier(name = "a"),
              value = Literal(value = Nullable("A")),
              kind = "init",
              method = false,
              shorthand = false,
              computed = false,
            ),
            Property(
              key = Identifier(name = "b"),
              value = Literal(value = Nullable("B")),
              kind = "init",
              method = false,
              shorthand = false,
              computed = false,
            ),
          ),
        )))(equalTo("{\n  a: \"A\",\n  b: \"B\"\n}"))
      },
    )

}
