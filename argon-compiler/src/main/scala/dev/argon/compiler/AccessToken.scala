package dev.argon.compiler

final case class AccessToken[Ctx <: Context](
  tube: TubeName,
  module: ModulePath,
  allowsAccessTo: Set[DeclarationBase & HasContext[Ctx]],
) {
  
  def add(decl: DeclarationBase & HasContext[Ctx]): AccessToken[Ctx] = copy(allowsAccessTo = allowsAccessTo + decl)
  
}
