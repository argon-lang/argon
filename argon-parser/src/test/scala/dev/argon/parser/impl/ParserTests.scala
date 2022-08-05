package dev.argon.parser.impl

import dev.argon.parser.{Token, ImportStmt, ImportPathSegment}
import zio.test.*
import zio.test.Assertion.*
import zio.*
import dev.argon.util.NonEmptyList

abstract class ArgonParserTestsCommon extends ZIOSpecDefault with GrammarTestHelpers with GrammarTokenHelpers {

  def suiteName: String

  override def spec: Spec[Environment & Scope, Any] =
    suite(suiteName)(
      test("Import statement parsing") {
        assert(
          parse(grammarFactory(ArgonParser.Rule.ImportStatement))(
            Token.KW_IMPORT,
            Token.Identifier("Ar"),
            Token.OP_SLASH,
            Token.OP_STAR,
          )
        )(
          isRight(equalTo((Chunk.empty, ImportStmt.Tube(NonEmptyList("Ar"), ImportPathSegment.Wildcard))))
        )
      }
    )

}

object ArgonParserTestsEntireSequence extends ArgonParserTestsCommon with GrammarTestHelpersEntireSequence {
  override def suiteName: String = "Parse entire sequence"
}

object ArgonParserTestsSingleTokens extends ArgonParserTestsCommon with GrammarTestHelpersSingleTokens {
  override def suiteName: String = "Parse with single tokens"
}
