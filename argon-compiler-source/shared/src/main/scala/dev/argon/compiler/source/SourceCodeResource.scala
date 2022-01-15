package dev.argon.compiler.source

import dev.argon.io.TextResource
import dev.argon.parser.{SyntaxError, Stmt}
import zio.stream.*
import java.io.IOException

trait SourceCodeResource extends TextResource {
  def parsed: Stream[IOException | SyntaxError, Stmt]

  override def asText: Stream[IOException, String]
}
