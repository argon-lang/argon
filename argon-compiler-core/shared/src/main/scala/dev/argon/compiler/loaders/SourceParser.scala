package dev.argon.compiler.loaders

import dev.argon.compiler.ErrorList
import dev.argon.parser.SourceAST
import dev.argon.util.FileSpec
import zio.stream._

object SourceParser {

  trait Service {
    def parse(fileSpec: FileSpec)(sourceCode: Stream[ErrorList, Char]): Stream[ErrorList, SourceAST]
  }

}
