package dev.argon.plugins.source

import dev.argon.parser
import dev.argon.util.WithSource
import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import zio.*

object AccessUtil {
  def parse(access: Seq[WithSource[parser.Modifier]]): IO[CompError, AccessModifier] = ???

  def parseGlobal(access: Seq[parser.Modifier]): IO[CompError, AccessModifierGlobal] = ???
}
