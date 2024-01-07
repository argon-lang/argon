package dev.argon.parser.impl

import dev.argon.grammar.Grammar
import dev.argon.parser.{SyntaxError, Token}
import dev.argon.util.FilePosition

trait GrammarTokenHelpers extends GrammarTestHelpers {
  override type TToken = Token
  override type TSyntaxError = SyntaxError
  override type TLabel[T] = ArgonParser.Rule.ArgonRuleName[T]

  protected override val grammarFactory: Grammar.GrammarFactory[Token, FilePosition, SyntaxError, ArgonParser.Rule.ArgonRuleName] =
    ArgonParser.ArgonGrammarFactory(None)

}
