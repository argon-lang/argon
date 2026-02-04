package dev.argon.compiler

final case class AccessToken[Ctx <: Context](
  tube: TubeName,
  module: ModulePath,
  allowsAccessTo: Seq[DeclarationBase & HasContext[Ctx]],
)
