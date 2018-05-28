package com.mi3software.argon.parser.impl

import com.mi3software.argon.parser.{SourceAST, Stmt}
import com.mi3software.argon.util.{FileSpec, NamespacePath, WithSource}

sealed trait TopLevelStatement
object TopLevelStatement {
  final case class Namespace(path: NamespacePath) extends TopLevelStatement
  final case class Import(path: NamespacePath) extends TopLevelStatement
  final case class Statement(stmt: WithSource[Stmt]) extends TopLevelStatement

  private final case class NSAndImports(ns: NamespacePath, imports: Vector[NamespacePath])

  def toSourceAST(fileSpec: FileSpec)(statements: Vector[TopLevelStatement]): Vector[SourceAST] =
    statements.foldLeft((Vector.empty[SourceAST], NSAndImports(NamespacePath.empty, Vector()))) {
      case ((acc, NSAndImports(_, imports)), Namespace(ns)) =>
        (acc, NSAndImports(ns, imports))

      case ((acc, NSAndImports(ns, imports)), Import(importNS)) =>
        (acc, NSAndImports(ns, imports :+ importNS))

      case ((acc, nsAndImports @ NSAndImports(ns, imports)), Statement(stmt)) =>
        (acc :+ SourceAST(fileSpec, ns, imports, stmt), nsAndImports)
    }._1

}
