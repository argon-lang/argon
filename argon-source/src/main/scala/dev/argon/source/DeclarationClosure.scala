package dev.argon.source

import dev.argon.ast
import dev.argon.compiler.{AccessToken, Context, ErasedSignature, ExternProvider, HasContext, ImportSpecifier, ModifierParser, UsingContext}
import zio.prelude.NonEmptyMap
import scala.compiletime.deferred

trait DeclarationClosure extends UsingContext {
  type Access
  def accessModifierParser: NonEmptyMap[Set[ast.Modifier], Access] 
  def getImportSpecifier(sig: ErasedSignature): ImportSpecifier
  
  given externProvider: ExternProvider & HasContext[context.type] = deferred
  
  def scope: context.Scopes.Scope
  def accessToken: AccessToken[context.type]
}
