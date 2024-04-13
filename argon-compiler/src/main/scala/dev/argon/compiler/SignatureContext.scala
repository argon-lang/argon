package dev.argon.compiler

import dev.argon.ast.{FunctionParameterListType, IdentifierExpr}
import dev.argon.expr.{BuiltinType, ExprContext}
import dev.argon.util.WithSource
import dev.argon.expr.Substitution

trait SignatureContext {
  val exprContext: ExprContext
  import exprContext.Expr

  final case class FunctionSignature(
    parameters: Seq[SignatureParameter],
    returnType: Expr,
    ensuresClauses: Seq[Expr],
  ) {
    def substituteVar(v: exprContext.Var, value: Expr): FunctionSignature =
      val mapping = Map(v -> value)
      FunctionSignature(
        parameters = parameters.map { p =>
          p.copy(paramType = Substitution.substitute(exprContext)(mapping)(p.paramType))
        },
        returnType = Substitution.substitute(exprContext)(mapping)(returnType),
        ensuresClauses = ensuresClauses.map(Substitution.substitute(exprContext)(mapping))
      )
    end substituteVar
  }
  
  final case class SignatureParameter(
    listType: FunctionParameterListType,
    isErased: Boolean,
    bindings: Seq[ParameterBinding],
    name: Option[IdentifierExpr],
    paramType: Expr,
  ) {
    
    def asParameterVar(owner: exprContext.ParameterOwner, index: Int): exprContext.ParameterVar =
      exprContext.ParameterVar(
        owner,
        parameterIndex = index,
        varType = paramType,
        name = name,
        isErased = isErased,
        isProof = listType == FunctionParameterListType.RequiresList,
      )
    
  }
  
  object SignatureParameter {
    import exprContext.ParameterVar

    def getParameterVariables(owner: exprContext.ParameterOwner, parameters: Seq[SignatureParameter]): Seq[ParameterVar] =
      parameters.zipWithIndex.map { (param, index) =>
        param.asParameterVar(owner, index)
      }
  }

  final case class ParameterBinding(
    name: Option[IdentifierExpr],
    paramType: Expr,
  )
  
}
