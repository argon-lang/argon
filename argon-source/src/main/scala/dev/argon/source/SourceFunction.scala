package dev.argon.source

import dev.argon.ast
import dev.argon.compiler.*
import dev.argon.expr.ErasureMode
import dev.argon.util.*
import zio.*

private[source] object SourceFunction {
  def make
  (ctx: Context)
  (closure: DeclarationClosure & HasContext[ctx.type])
  (decl: ast.FunctionDeclarationStmt)
  : ctx.Comp[DeclarationResult[closure.Access, ArFuncC & HasContext[ctx.type]]] =
    for
      funcId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      implCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.implementations.FunctionImplementation]

      mp <- ModifierParser.make(decl.modifiers, decl.name.location)
      access <- mp.parse(closure.accessModifierParser)
      inlineFlag <- mp.parse(ModifierParser.isInline)
      witnessFlag <- mp.parse(ModifierParser.isWitness)
      erasure <- mp.parse(ModifierParser.erasureModeWithToken)
      _ <- mp.done

      _ <- ErrorLog.logError(CompilerError.ErasedMustBePure(decl.name.location))
        .whenDiscard(erasure == ErasureMode.Erased && !decl.purity)

      _ <- ErrorLog.logError(CompilerError.TokenFunctionNotInline(decl.name.location))
        .whenDiscard(erasure == ErasureMode.Token && !inlineFlag)
      
    yield DeclarationResult(
      access,
      new ArFuncC {
        override val context: ctx.type = ctx
        override val id: UniqueIdentifier = funcId

        override def isInline: Boolean = inlineFlag
        override def erasureMode: ErasureMode.Declared = erasure
        override def isWitness: Boolean = witnessFlag

        override def effects: context.DefaultExprContext.EffectInfo =
          if decl.purity then context.DefaultExprContext.EffectInfo.Pure
          else context.DefaultExprContext.EffectInfo.Effectful

        override def importSpecifier: Comp[ImportSpecifier] =
          for
            sig <- signature
            erasedSig <- SignatureEraser(ctx).eraseSignature(sig)
          yield closure.getImportSpecifier(erasedSig)

        override def signature: Comp[FunctionSignature] = sigCache.get {
          val scope = closure.scope
          SourceSignature.parse(ctx)(scope)(closure.accessToken)(context.TRExprContext.ExpressionOwner.Func(this))(decl.parameters, decl.returnType)
        }

        override def implementation: Option[Comp[context.implementations.FunctionImplementation]] =
          Some(implCache.get(
            for
              scope = closure.scope
              sig <- signature
              scope2 = context.Scopes.ParameterScope(context.TRExprContext.ExpressionOwner.Func(this), scope, sig.parameters)
              impl <- decl.body match {
                case ast.FunctionBody.ExternBody(WithLocation(name, loc)) =>
                  closure.externProvider.getExternFunction(name)
                    .flatMap {
                      case Some(ext) =>
                        ZIO.succeed(ctx.implementations.FunctionImplementation.Extern(ext))

                      case None =>
                        ErrorLog.logError(CompilerError.UnknownExtern(loc, name))
                          .as(context.implementations.FunctionImplementation.Expr(
                            context.DefaultExprContext.Expr.Error()
                          ))
                    }


                case ast.FunctionBody.ExprBody(expr) =>
                  val tr = new TypeResolver {
                    override val context: ctx.type = ctx
                  }

                  tr.typeCheckExpr(scope2)(expr, sig.returnType, effects, erasure, closure.accessToken)
                    .map(context.implementations.FunctionImplementation.Expr.apply)
              }
            yield impl
          ))

        override def toString(): String =
          decl.name.toString()

      },
    )
}
