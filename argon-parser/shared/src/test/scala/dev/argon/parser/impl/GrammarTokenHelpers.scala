package dev.argon.parser.impl

import dev.argon.grammar.Grammar
import dev.argon.parser.{SyntaxError, Token}

trait GrammarTokenHelpers extends GrammarTestHelpers {
  override type TToken = Token
  override type TSyntaxError = SyntaxError
  override type TLabel = ArgonParser.Rule.ArgonRuleName

  override protected val grammarFactory: Grammar.GrammarFactory[Token, SyntaxError, ArgonParser.Rule.ArgonRuleName] =
    ArgonParser.grammarFactory
}
