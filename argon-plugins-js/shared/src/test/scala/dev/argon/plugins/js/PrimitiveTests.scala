package dev.argon.plugins.js

import dev.argon.util.*
import dev.argon.grammar.Grammar
import Grammar.Operators.*
import scala.language.postfixOps
import zio.*
import zio.test.*
import zio.test.Assertion.*

import estree.*

object PrimitiveTests extends ZIOSpecDefault with GenerateTestsHelper {

  override def spec: Spec[Environment & Scope, Any] =
    suite("Primitive AST encoding")(
      test("String") {
        assertZIO(generate[Literal](Literal(
          value = Nullable("a\nb")
        )))(equalTo("\"a\\nb\""))
      },
      test("Null") {
        assertZIO(generate[Literal](Literal(
          value = Nullable(null)
        )))(equalTo("null"))
      },
      test("Number (Integral)") {
        assertZIO(generate[Literal](Literal(
          value = Nullable(4 : Double)
        )))(equalTo("4"))
      },
      test("Number (Fractional)") {
        assertZIO(generate[Literal](Literal(
          value = Nullable(4.5 : Double)
        )))(equalTo("4.5"))
      },
      test("Boolean") {
        assertZIO(generate[Literal](Literal(
          value = Nullable(true)
        )))(equalTo("true"))
      },
      test("BigInt") {
        assertZIO(generate[Literal](Literal(
          value = Nullable(999: BigInt),
          bigint = Some("999"),
        )))(equalTo("999n"))
      },
    )

}
