package dev.argon.compiler.module

import dev.argon.grammar.Grammar
import dev.argon.grammar.Grammar.Operators.*
import dev.argon.util.*
import zio.*
import zio.test.*
import zio.test.Assertion.*


object ModulePathTests extends ZIOSpecDefault {

  override def spec: Spec[Environment & Scope, Any] =
    suite("ModulePath urlDecode")(
      test("Empty path") {
        assertTrue(ModulePath.urlDecode("") == ModulePath(Seq.empty))
      },
      test("1 segment path") {
        assertTrue(ModulePath.urlDecode("Bool") == ModulePath(Seq("Bool")))
      },
      test("1 segment path with slash") {
        assertTrue(ModulePath.urlDecode("Bo%2Fol") == ModulePath(Seq("Bo/ol")))
      },
      test("1 segment path with slash (lowercase)") {
        assertTrue(ModulePath.urlDecode("Bo%2fol") == ModulePath(Seq("Bo/ol")))
      },
      test("1 segment path with dollar") {
        assertTrue(ModulePath.urlDecode("Bo%24ol") == ModulePath(Seq("Bo$ol")))
      },
      test("2 segment path") {
        assertTrue(ModulePath.urlDecode("ABC/XYZ") == ModulePath(Seq("ABC", "XYZ")))
      },
      test("2 segment path with slash") {
        assertTrue(ModulePath.urlDecode("A%2FBC/XYZ") == ModulePath(Seq("A/BC", "XYZ")))
      },
    ) +
      suite("ModulePath urlDecode")(
        test("Empty path") {
          assertTrue(ModulePath(Seq.empty).urlEncode == "")
        },
        test("1 segment path") {
          assertTrue(ModulePath(Seq("Bool")).urlEncode == "Bool")
        },
        test("1 segment path with slash") {
          assertTrue(ModulePath(Seq("Bo/ol")).urlEncode == "Bo%2Fol")
        },
        test("1 segment path with dollar") {
          assertTrue(ModulePath(Seq("Bo$ol")).urlEncode == "Bo$ol")
        },
        test("2 segment path") {
          assertTrue(ModulePath(Seq("ABC", "XYZ")).urlEncode == "ABC/XYZ")
        },
        test("2 segment path with slash") {
          assertTrue(ModulePath(Seq("A/BC", "XYZ")).urlEncode == "A%2FBC/XYZ")
        },
      )
}
