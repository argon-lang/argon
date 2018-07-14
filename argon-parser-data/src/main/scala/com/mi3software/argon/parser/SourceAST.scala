package com.mi3software.argon.parser

import com.mi3software.argon.util.{FileSpec, NamespacePath, WithSource}

final case class SourceAST(fileSpec: FileSpec, currentNamespace: NamespacePath, importNamespaces: Vector[NamespacePath], statement: WithSource[Stmt])
