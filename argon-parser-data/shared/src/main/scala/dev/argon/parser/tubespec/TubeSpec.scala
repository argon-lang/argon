package dev.argon.parser.tubespec

import dev.argon.parser.Token.StringToken
import dev.argon.parser.IdentifierExpr

final case class TubeSpec(
  modules: Seq[ModulePatternMapping],
)


final case class ModulePatternMapping(
  module: Seq[ModulePatternSegment],
  fileNameTemplate: StringToken,
)


enum ModulePatternSegment {
  case Named(name: String)
  case Star(boundName: IdentifierExpr)
  case DoubleStar(boundName: IdentifierExpr)
}

