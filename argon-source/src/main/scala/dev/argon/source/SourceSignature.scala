package dev.argon.source

import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.{CompilerError, Context, ErrorLog, ModifierParser, TypeResolver}
import dev.argon.expr.ErasureMode
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
      case ctx.TRExprContext.ExpressionOwner.Func(f) =>
        f.erasureMode == ErasureMode.Erased
        
      case ctx.TRExprContext.ExpressionOwner.Rec(_) => false
      case ctx.TRExprContext.ExpressionOwner.Enum(_) => false
      case ctx.TRExprContext.ExpressionOwner.EnumVariant(_) => false
      case ctx.TRExprContext.ExpressionOwner.Trait(_) => false
      case ctx.TRExprContext.ExpressionOwner.Method(_) => false
      case ctx.TRExprContext.ExpressionOwner.Instance(_) => false
    }

    val allowConcreteParams = owner match {
      case ctx.TRExprContext.ExpressionOwner.Func(f) => f.erasureMode != ErasureMode.Token
      case ctx.TRExprContext.ExpressionOwner.Rec(_) => false
      case ctx.TRExprContext.ExpressionOwner.Enum(_) => false
      case ctx.TRExprContext.ExpressionOwner.EnumVariant(_) => true
      case ctx.TRExprContext.ExpressionOwner.Trait(_) => false
      case ctx.TRExprContext.ExpressionOwner.Method(_) => true
      case ctx.TRExprContext.ExpressionOwner.Instance(_) => true
    }

    def impl(remainingParams: Seq[WithSource[ast.FunctionParameterList]], convParams: Seq[SignatureParameter]): Comp[FunctionSignature] =
      remainingParams match {
        case head +: tail =>
          for
            mp <- ModifierParser.make(head.value.modifiers, head.location)
            erasureMode <- mp.parse(ModifierParser.erasureModeWithToken)
            _ <- mp.done

            _ <- ErrorLog.logError(CompilerError.TypeParameterIsConcrete(head.location))
              .whenDiscard(!allowConcreteParams && erasureMode == ErasureMode.Concrete)
            
            bindings <- ZIO.foreach(head.value.parameters) { param =>
              for
                t <- tr.typeCheckTypeExpr(ParameterScope(owner, scope, convParams))(param.value.paramType, erased = ownerIsErased || erasureMode == ErasureMode.Erased)
              yield ParameterBinding(
                name = Some(param.value.name),
                paramType = t,
              )
            }

            singleBinding = bindings match {
              case Seq(binding) if !head.value.hasTrailingComma => Some(binding)
              case _ => None
            }
            param = SignatureParameter(
              listType = head.value.listType,
              erasureMode = erasureMode,
              bindings = bindings,
              name = singleBinding.flatMap(_.name),
              paramType = singleBinding.fold(Expr.Tuple(bindings.map(_.paramType)))(_.paramType),
            )
            
            res <- impl(tail, convParams :+ param)
          yield res

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
