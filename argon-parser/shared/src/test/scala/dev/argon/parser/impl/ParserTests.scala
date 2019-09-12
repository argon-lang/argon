package dev.argon.parser.impl

import dev.argon.grammar.Grammar
import dev.argon.parser.impl.ArgonParser.Rule
import dev.argon.parser.{SyntaxError, Token}
import dev.argon.util.NamespacePath
import zio.test._
import zio.test.Assertion._

abstract class ArgonParserTestsCommon extends GrammarTestHelpers with GrammarTokenHelpers {

  def parserTestSuite(suiteName: String): ZSpec[Any, Nothing, String, Any] =
    suite(suiteName)(
      test("Import statement parsing") {
        assert(
          parse(ArgonParser.grammarFactory(ArgonParser.Rule.ImportNamespace))(
            Token.KW_IMPORT, Token.Identifier("Ar"), Token.OP_DOT, Token.KW_UNDERSCORE
          ),
          isRight(equalTo((Vector[TToken](), TopLevelStatement.Import(NamespacePath(Vector("Ar"))) : TopLevelStatement)))
        )
      }
    )

}

object ArgonParserTestsEntireSequence extends DefaultRunnableSpec(
  new ArgonParserTestsCommon with GrammarTestHelpersEntireSequence {}
    .parserTestSuite("Parse entire sequence")
)
object ArgonParserTestsSingleTokens extends DefaultRunnableSpec(
  new ArgonParserTestsCommon with GrammarTestHelpersSingleTokens {}
    .parserTestSuite("Parse with single tokens")
)

