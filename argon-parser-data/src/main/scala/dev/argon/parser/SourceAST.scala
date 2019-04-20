package dev.argon.parser

import dev.argon.util.{FileSpec, NamespacePath, WithSource}

final case class SourceAST(fileSpec: FileSpec, index: Int, currentNamespace: NamespacePath, importNamespaces: Vector[NamespacePath], statement: WithSource[Stmt])
