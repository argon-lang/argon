package dev.argon.source

import dev.argon.ast
import dev.argon.compiler.*
import zio.prelude.NonEmptyMap

import scala.compiletime.deferred

trait MethodClosure extends UsingContext {
  type Access
  def accessModifierParser: NonEmptyMap[Set[ast.Modifier], Access]

  def methodOwner: MethodOwner[context.type]

  given externProvider: ExternProvider & HasContext[context.type] = deferred
  
  def scope: context.Scopes.Scope
  def accessToken: AccessToken & HasContext[context.type]
}
