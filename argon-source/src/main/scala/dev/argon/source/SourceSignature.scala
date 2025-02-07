package dev.argon.source

import dev.argon.ast
import dev.argon.compiler.{Context, TypeResolver}
import dev.argon.util.WithSource
import zio.*

object SourceSignature {
  def parse
  (ctx: Context)
  (scope: ctx.Scopes.Scope)
  (owner: ctx.TRExprContext.ParameterOwner)
  (parameters: Seq[WithSource[ast.FunctionParameterList]], returnType: WithSource[ast.ReturnTypeSpecifier])
  : ctx.Comp[ctx.DefaultSignatureContext.FunctionSignature] =
    import ctx.Comp
    import ctx.DefaultSignatureContext.*
    import ctx.Scopes.*
    import ctx.DefaultExprContext.Expr

    val tr = new TypeResolver {
      override val context: ctx.type = ctx
    }

    def impl(remainingParams: Seq[WithSource[ast.FunctionParameterList]], convParams: Seq[SignatureParameter]): Comp[FunctionSignature] =
      remainingParams match {
        case head +: tail =>
          ZIO.foreach(head.value.parameters) { param =>
            for
              t <- tr.typeCheckTypeExpr(ParameterScope(owner, scope, convParams))(param.value.paramType)
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
            returnTypeExpr <- tr.typeCheckTypeExpr(ParameterScope(owner, scope, convParams))(returnType.value.returnType)
            ensuresClauses <- ZIO.foreach(returnType.value.ensuresClauses)(tr.typeCheckTypeExpr(ParameterScope(owner, scope, convParams)))
          yield FunctionSignature(
            parameters = convParams,
            returnType = returnTypeExpr,
            ensuresClauses = ensuresClauses,
          )
      }

    impl(parameters, Seq.empty)
  end parse
}
