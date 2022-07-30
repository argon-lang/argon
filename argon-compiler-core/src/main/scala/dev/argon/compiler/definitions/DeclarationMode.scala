package dev.argon.compiler.definitions

trait DeclarationMode {
  type IsDeclaration <: Boolean
}

type HasDeclaration[IsDecl <: Boolean] = DeclarationMode { type IsDeclaration = IsDecl }
