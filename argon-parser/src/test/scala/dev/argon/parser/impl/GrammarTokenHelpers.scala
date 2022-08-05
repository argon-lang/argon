package dev.argon.parser.impl

import dev.argon.grammar.Grammar
import dev.argon.parser.{SyntaxError, Token}

trait GrammarTokenHelpers extends GrammarTestHelpers {
  override type TToken = Token
  override type TSyntaxError = SyntaxError
  override type TLabel[T] = ArgonParser.Rule.ArgonRuleName[T]

  protected override val grammarFactory: Grammar.GrammarFactory[Token, SyntaxError, ArgonParser.Rule.ArgonRuleName] =
    ArgonParser.ArgonGrammarFactory(None)

}
