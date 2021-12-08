package dev.argon.compiler

sealed trait DefinitionKind
sealed trait Declaration extends DefinitionKind
sealed trait Reference extends DefinitionKind

type ForDeclaration[Kind <: DefinitionKind, Decl, Ref] = Kind match {
  case Declaration => Decl
  case Reference => Ref
}
