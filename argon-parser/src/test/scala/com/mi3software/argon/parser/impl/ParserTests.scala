package com.mi3software.argon.parser.impl

import com.mi3software.argon.grammar.Grammar
import com.mi3software.argon.parser.impl.ArgonParser.Rule
import com.mi3software.argon.parser.{SyntaxError, Token}
import com.mi3software.argon.util.NamespacePath
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

