package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.Token
import com.mi3software.argon.util.NamespacePath
import org.scalatest.{FlatSpec, Matchers}

abstract class ParserTestsCommon extends FlatSpec with Matchers with GrammarTestHelpers with GrammarTokenHelpers {

  val parser = new Parser

  "An import statement" should "parse correctly" in {
    parse(parser.ruleImportNamespace)(
      Token.KW_IMPORT, Token.Identifier("Ar"), Token.OP_DOT, Token.KW_UNDERSCORE
    ) shouldBe Right((Vector(), TopLevelStatement.Import(NamespacePath(Vector("Ar")))))
  }

}

class ParserTestsEntireSequence extends ParserTestsCommon with GrammarTestHelpersEntireSequence
class ParserTestsSingleTokens extends ParserTestsCommon with GrammarTestHelpersSingleTokens

