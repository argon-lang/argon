package dev.argon.compiler.loaders

import dev.argon.compiler.CompilationError
import dev.argon.parser.SourceAST
import dev.argon.util.FileSpec
import zio.stream._

object SourceParser {

  trait Service {
    def parse(fileSpec: FileSpec)(sourceCode: Stream[CompilationError, Char]): Stream[CompilationError, SourceAST]
  }

}
