package dev.argon.parser

private[parser] enum TokenType derives CanEqual {
  case Token(t: dev.argon.parser.Token)
  
  case Whitespace
  
  case StringStart
  
  case BinInteger, OctInteger, DecInteger, HexInteger
  case Identifier
  case InvalidInteger
}
