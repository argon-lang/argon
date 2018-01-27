package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.Expr
import com.mi3software.argon.util.{NamespacePath, WithSource}

sealed trait TopLevelStatement
object TopLevelStatement {
  final case class Namespace(path: NamespacePath) extends TopLevelStatement
  final case class Import(path: NamespacePath) extends TopLevelStatement
  final case class Expression(expr: WithSource[Expr]) extends TopLevelStatement
}
