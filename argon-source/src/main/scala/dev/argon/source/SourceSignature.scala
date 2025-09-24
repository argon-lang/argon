package dev.argon.source

import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.{Context, TypeResolver}
import dev.argon.util.{WithLocation, WithSource}
import zio.*

object SourceSignature {
  def parse
  (ctx: Context)
  (scope: ctx.Scopes.Scope)
  (owner: ctx.TRExprContext.ExpressionOwner)
  (parameters: Seq[WithSource[ast.FunctionParameterList]], returnType: WithSource[ast.ReturnTypeSpecifier])
  : ctx.Comp[ctx.DefaultSignatureContext.FunctionSignature] =
    import ctx.Comp
    import ctx.DefaultSignatureContext.*
    import ctx.Scopes.*
    import ctx.DefaultExprContext.Expr

    val tr = new TypeResolver {
      override val context: ctx.type = ctx
    }

    val ownerIsErased = owner match {
      case ctx.TRExprContext.ExpressionOwner.Func(f) => f.isErased
      case ctx.TRExprContext.ExpressionOwner.Rec(_) => false
      case ctx.TRExprContext.ExpressionOwner.Enum(_) => false
      case ctx.TRExprContext.ExpressionOwner.EnumVariant(_) => false
      case ctx.TRExprContext.ExpressionOwner.Trait(_) => false
      case ctx.TRExprContext.ExpressionOwner.Method(_) => false
      case ctx.TRExprContext.ExpressionOwner.Instance(_) => false
    }

    def impl(remainingParams: Seq[WithSource[ast.FunctionParameterList]], convParams: Seq[SignatureParameter]): Comp[FunctionSignature] =
      remainingParams match {
        case head +: tail =>
          ZIO.foreach(head.value.parameters) { param =>
            for
              t <- tr.typeCheckTypeExpr(ParameterScope(owner, scope, convParams))(param.value.paramType, erased = ownerIsErased || head.value.isErased)
            yield ParameterBinding(
              name = Some(param.value.name),
              paramType = t,
            )
          }
            .flatMap { bindings =>
              val singleBinding = bindings match {
                case Seq(binding) if !head.value.hasTrailingComma => Some(binding)
                case _ => None
              }


              val param = SignatureParameter(
                listType = head.value.listType,
                isErased = head.value.isErased,
                bindings = bindings,
                name = singleBinding.flatMap(_.name),
                paramType = singleBinding.fold(Expr.Tuple(bindings.map(_.paramType)))(_.paramType),
              )

              impl(tail, convParams :+ param)
            }

        case _ =>
          for
            returnTypeExpr <- tr.typeCheckTypeExpr(ParameterScope(owner, scope, convParams))(returnType.value.returnType, erased = ownerIsErased)
            ensuresClauses <- ZIO.foreach(returnType.value.ensuresClauses)(tr.typeCheckTypeExpr(ParameterScope(owner, scope, convParams))(_, erased = true))
          yield FunctionSignature(
            parameters = convParams,
            returnType = returnTypeExpr,
            ensuresClauses = ensuresClauses,
          )
      }

    impl(parameters, Seq.empty)
  end parse
  
  def exprToReturnType(expr: WithSource[ast.Expr]): WithSource[ast.ReturnTypeSpecifier] =
    WithLocation(
      ast.ReturnTypeSpecifier(
        returnType = expr,
        ensuresClauses = Seq(),
      ),
      expr.location,
    )
  
  def getTypeSigReturnType(name: WithSource[IdentifierExpr], rt: Option[WithSource[ast.Expr]]): WithSource[ast.ReturnTypeSpecifier] =
    rt match {
      case Some(rt) =>
        WithLocation(
          ast.ReturnTypeSpecifier(
            returnType = rt,
            ensuresClauses = Seq(),
          ),
          rt.location,
        )

      case None =>
        WithLocation(
          ast.ReturnTypeSpecifier(
            returnType = WithLocation(
              ast.Expr.Type,
              name.location,
            ),
            ensuresClauses = Seq(),
          ),
          name.location,
        )
    }
  
}
