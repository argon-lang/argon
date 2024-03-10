package dev.argon.compiler

import dev.argon.ast.{FunctionParameterListType, IdentifierExpr}
import dev.argon.expr.{BuiltinType, ExprContext}
import dev.argon.util.WithSource

trait SignatureContext {
  val exprContext: ExprContext
  import exprContext.Expr

  final case class FunctionSignature(
    parameters: Seq[SignatureParameter],
    returnType: Expr,
    ensuresClauses: Seq[Expr],
  )
  
  final case class SignatureParameter(
    listType: FunctionParameterListType,
    isErased: Boolean,
    bindings: Seq[ParameterBinding],
    name: Option[IdentifierExpr],
    paramType: Expr,
  )
  
  object SignatureParameter {
    import exprContext.ParameterVar

    def getParameterVariables(owner: exprContext.ParameterOwner, parameters: Seq[SignatureParameter]): Seq[ParameterVar] =
      parameters.zipWithIndex.flatMap { (param, index) =>
        val isProof = param.listType == FunctionParameterListType.RequiresList
        ParameterVar(
          owner,
          parameterIndex = index,
          tupleIndex = None,
          varType = param.paramType,
          name = param.name,
          isErased = param.isErased,
          isProof = isProof,
        ) +: param.bindings.zipWithIndex.map { (binding, bindingIndex) =>
          ParameterVar(
            owner,
            parameterIndex = index,
            tupleIndex = Some(bindingIndex),
            varType = binding.paramType,
            name = binding.name,
            isErased = param.isErased,
            isProof = isProof,
          )
        }
      }
  }

  final case class ParameterBinding(
    name: Option[IdentifierExpr],
    paramType: Expr,
  )
  
}
