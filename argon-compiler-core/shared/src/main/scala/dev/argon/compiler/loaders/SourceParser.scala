package dev.argon.compiler.loaders

import dev.argon.compiler.CompError
import dev.argon.parser.SourceAST
import dev.argon.util.FileSpec
import zio.stream._

object SourceParser {

  trait Service {
    def parse(fileSpec: FileSpec)(sourceCode: Stream[CompError, Char]): Stream[CompError, SourceAST]
  }

}
