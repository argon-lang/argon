package dev.argon.parser.impl

import dev.argon.parser.Token
import dev.argon.util.NamespacePath
import zio.test._
import zio.test.Assertion._

abstract class ArgonParserTestsCommon extends DefaultRunnableSpec with GrammarTestHelpers with GrammarTokenHelpers {

  def suiteName: String

  override def spec: ZSpec[Environment, Failure] =
    suite(suiteName)(
      test("Import statement parsing") {
        assert(
          parse(ArgonParser.grammarFactory(ArgonParser.Rule.ImportNamespace))(
            Token.KW_IMPORT, Token.Identifier("Ar"), Token.OP_DOT, Token.KW_UNDERSCORE
          )
        )(
          isRight(equalTo((Vector[TToken](), TopLevelStatement.Import(NamespacePath(Vector("Ar"))) : TopLevelStatement)))
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

