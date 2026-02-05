package dev.argon.compiler

final case class AccessRequest[Ctx <: Context](
  decl: DeclarationBase & HasContext[Ctx],
  instanceDecl: Option[DeclarationBase & HasContext[Ctx]],
  accessModifier: AccessModifier,
)
