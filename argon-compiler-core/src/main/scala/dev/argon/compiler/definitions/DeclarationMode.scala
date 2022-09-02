package dev.argon.compiler.definitions

trait DeclarationMode {
  type IsImplementation <: Boolean
}

type HasImplementation[IsDecl <: Boolean] = DeclarationMode { type IsImplementation = IsDecl }
