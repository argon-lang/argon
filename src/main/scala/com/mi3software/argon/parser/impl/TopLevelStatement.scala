package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.Stmt
import com.mi3software.argon.util.{NamespacePath, WithSource}

sealed trait TopLevelStatement
object TopLevelStatement {
  final case class Namespace(path: NamespacePath) extends TopLevelStatement
  final case class Import(path: NamespacePath) extends TopLevelStatement
  final case class Statement(stmt: WithSource[Stmt]) extends TopLevelStatement
}
