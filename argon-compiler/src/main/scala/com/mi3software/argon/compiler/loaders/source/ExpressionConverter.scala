package com.mi3software.argon.compiler.loaders.source

import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.lookup._
import com.mi3software.argon.parser
import com.mi3software.argon.util.{FileSpec, WithSource}

sealed trait ExpressionConverter extends VariableContext with SignatureContext with ScopeContext {

  type Env = ExpressionConverter.Env[Scope]

}

object ExpressionConverter {

  final case class Env[TScope]
  (
    descriptor: Descriptor,
    scope: TScope,
    fileSpec: FileSpec
  )

  def convertExpression[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (env: Env[context.Scope])
  (expectedType: context.typeSystem.TType)
  (expr: WithSource[parser.Expr])
  : TComp[context.ArExpr] = ???

  def convertTypeExpression[TComp[+_] : Compilation]
  (context: ContextComp[TComp])
  (env: Env[context.Scope])
  (expr: WithSource[parser.Expr])
  : TComp[context.typeSystem.TType] = ???

}
