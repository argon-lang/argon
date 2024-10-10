package esexpr.parser

enum ESExprToken derives CanEqual {
  case OpenParen, CloseParen, Colon

  case Identifier(name: String)
  case Float32Literal(f: Float)
  case Float64Literal(d: Double)
  case IntegerLiteral(i: BigInt)
  case StringLiteral(s: String)
  case TrueAtom
  case FalseAtom
  case NullAtom(level: BigInt)
}
