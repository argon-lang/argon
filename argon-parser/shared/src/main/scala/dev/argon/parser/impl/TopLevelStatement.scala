package dev.argon.parser.impl

import dev.argon.parser.{SourceAST, Stmt}
import dev.argon.util.{FileSpec, NamespacePath, WithSource}

sealed trait TopLevelStatement
object TopLevelStatement {
  final case class Namespace(path: NamespacePath) extends TopLevelStatement
  final case class Import(path: NamespacePath) extends TopLevelStatement
  final case class Statement(stmt: WithSource[Stmt]) extends TopLevelStatement

  final case class NSAndImports(index: Int, ns: NamespacePath, imports: Vector[NamespacePath])

  val defaultNSAndImports = NSAndImports(1, NamespacePath.empty, Vector())

  def accumulate(fileSpec: FileSpec)(nsi: NSAndImports, stmt: TopLevelStatement): (NSAndImports, Option[SourceAST]) =
    stmt match {
      case Namespace(ns) =>
        (nsi.copy(ns = ns), None)

      case Import(importNS) =>
        (nsi.copy(imports = nsi.imports :+ importNS), None)

      case Statement(stmt) =>
        (nsi.copy(index = nsi.index + 1), Some(SourceAST(fileSpec, nsi.index, nsi.ns, nsi.imports, stmt)))
    }

  def toSourceAST(fileSpec: FileSpec)(statements: Vector[TopLevelStatement]): Vector[SourceAST] =
    statements.foldLeft((Vector.empty[SourceAST], defaultNSAndImports)) {
      case ((acc, state), tls) =>
        val (newState, stmt) = accumulate(fileSpec)(state, tls)
        (acc ++ stmt.toList, newState)
    }._1

}
