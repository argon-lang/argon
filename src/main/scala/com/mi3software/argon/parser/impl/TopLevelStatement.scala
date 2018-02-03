package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.{SourceAST, Stmt}
import com.mi3software.argon.util.{FileSpec, NamespacePath, WithSource}

sealed trait TopLevelStatement
object TopLevelStatement {
  final case class Namespace(path: NamespacePath) extends TopLevelStatement
  final case class Import(path: NamespacePath) extends TopLevelStatement
  final case class Statement(stmt: WithSource[Stmt]) extends TopLevelStatement

  private final case class NSAndImports(ns: NamespacePath, imports: Vector[NamespacePath])

  def toSourceAST(fileSpec: FileSpec)(statements: Vector[WithSource[TopLevelStatement]]): Vector[WithSource[SourceAST]] =
    statements.foldLeft((Vector.empty[WithSource[SourceAST]], NSAndImports(NamespacePath.empty, Vector()))) {
      case ((acc, NSAndImports(_, imports)), WithSource(Namespace(ns), _)) =>
        (acc, NSAndImports(ns, imports))

      case ((acc, NSAndImports(ns, imports)), WithSource(Import(importNS), _)) =>
        (acc, NSAndImports(ns, imports :+ importNS))

      case ((acc, nsAndImports @ NSAndImports(ns, imports)), WithSource(Statement(stmt), location)) =>
        (acc :+ WithSource(SourceAST(fileSpec, ns, imports, stmt), location), nsAndImports)
    }._1

}
