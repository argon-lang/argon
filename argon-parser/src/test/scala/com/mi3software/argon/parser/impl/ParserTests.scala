package dev.argon.parser.impl

import dev.argon.grammar.Grammar
import dev.argon.parser.impl.ArgonParser.Rule
import dev.argon.parser.{SyntaxError, Token}
import dev.argon.util.NamespacePath
import org.scalatest.{FlatSpec, Matchers}

abstract class ArgonParserTestsCommon extends FlatSpec with Matchers with GrammarTestHelpers with GrammarTokenHelpers {

  "An import statement" should "parse correctly" in {
    parse(ArgonParser.grammarFactory(ArgonParser.Rule.ImportNamespace))(
      Token.KW_IMPORT, Token.Identifier("Ar"), Token.OP_DOT, Token.KW_UNDERSCORE
    ) shouldBe Right((Vector(), TopLevelStatement.Import(NamespacePath(Vector("Ar")))))
  }
}

class ArgonParserTestsEntireSequence extends ArgonParserTestsCommon with GrammarTestHelpersEntireSequence
class ArgonParserTestsSingleTokens extends ArgonParserTestsCommon with GrammarTestHelpersSingleTokens

