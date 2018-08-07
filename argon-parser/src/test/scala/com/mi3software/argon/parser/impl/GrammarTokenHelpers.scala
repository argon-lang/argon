package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.{SyntaxError, Token}

trait GrammarTokenHelpers extends GrammarTestHelpers {
  override type TToken = Token
  override type TSyntaxError = SyntaxError
  override type TLabel = String
}
