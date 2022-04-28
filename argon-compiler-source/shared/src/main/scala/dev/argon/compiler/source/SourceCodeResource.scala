package dev.argon.compiler.source

import dev.argon.compiler.*
import dev.argon.io.TextResource
import dev.argon.parser.{SyntaxError, Stmt}
import zio.stream.*
import java.io.IOException

trait SourceCodeResource extends TextResource[CompError] {
  def parsed: Stream[CompError, Stmt]

  override def asText: Stream[CompError, String]
}
