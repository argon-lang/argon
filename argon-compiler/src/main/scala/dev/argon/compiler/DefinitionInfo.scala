package dev.argon.compiler

import dev.argon.ast.IdentifierExpr

enum DefinitionInfo {
  case Global(tube: TubeName, modulePath: ModulePath, name: Option[IdentifierExpr], sig: ErasedSignature)
} 
