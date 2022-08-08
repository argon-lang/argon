package dev.argon.compiler.module

import dev.argon.parser.IdentifierExpr

final case class ModulePath(ids: Seq[String]) derives CanEqual {
  override def toString: String =
    ids.mkString("/")
}
