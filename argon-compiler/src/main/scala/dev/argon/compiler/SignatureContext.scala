package dev.argon.compiler

import dev.argon.ast.{FunctionParameterListType, IdentifierExpr}
import dev.argon.expr.{BuiltinType, ExprContext}
import dev.argon.util.WithSource
import dev.argon.expr.Substitution
import dev.argon.util.{*, given}

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


    def returnTypeForArgs(owner: exprContext.ParameterOwner, args: Seq[Expr]): Expr =
      substituteWithinExprForArgs(owner, args, returnType)


    def substituteWithinExprForArgs(owner: exprContext.ParameterOwner, args: Seq[Expr], e: Expr): Expr =
      def impl(index: Int, remaining: List[(SignatureParameter, Expr)], e: Expr): Expr =
        remaining match {
          case (param, arg) :: next =>
            val paramVar = param.asParameterVar(owner, index)
            impl(
              index + 1,
              next.map { (param, arg) => (param, Substitution.substitute(exprContext)(Map(paramVar -> arg))(arg)) },
              Substitution.substitute(exprContext)(Map(paramVar -> arg))(e)
            )

          case Nil => e
        }

      impl(0, parameters.iterator.zip(args).toList, e)
    end substituteWithinExprForArgs
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
