package dev.argon.source

import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.*
import dev.argon.expr.ErasureMode
import dev.argon.util.*
import zio.*

private[source] object SourceMethod {
  def make(ctx: Context)(closure: MethodClosure & HasContext[ctx.type])(decl: ast.MethodDeclarationStmt): ctx.Comp[DeclarationResult[closure.Access, ArMethodC & HasContext[ctx.type]]] =
    for
      funcId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      implCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.implementations.MethodImplementation]

      mp <- ModifierParser.make(decl.modifiers, decl.name.location)
      access <- mp.parse(closure.accessModifierParser)
      inlineFlag <- mp.parse(ModifierParser.isInline)
      witnessFlag <- mp.parse(ModifierParser.isWitness)
      erasure <- mp.parse(ModifierParser.erasureModeWithoutToken)
      methodSlot <- mp.parse(
        if decl.body.isEmpty then
          ModifierParser.methodSlotAbstract
        else
          ModifierParser.methodSlotConcrete
      )
      _ <- mp.done
      

      _ <- ErrorLog.logError(CompilerError.ErasedMustBePure(decl.name.location))
        .whenDiscard(erasure == ErasureMode.Erased && !decl.purity)
      
    yield DeclarationResult(
      access,
      new ArMethodC {
        override val context: ctx.type = ctx
        override val name: IdentifierExpr = decl.name.value
        override val id: UniqueIdentifier = funcId
        override val owner: MethodOwner[context.type] = closure.methodOwner

        override def isInline: Boolean = inlineFlag
        override def erasureMode: ErasureMode.DeclaredNonToken = erasure
        override def isWitness: Boolean = witnessFlag

        override def effects: context.DefaultExprContext.EffectInfo =
          if decl.purity then context.DefaultExprContext.EffectInfo.Pure
          else context.DefaultExprContext.EffectInfo.Effectful

        override val slot: MethodSlot = methodSlot

        override def instanceParam: Comp[context.DefaultSignatureContext.InstanceParameter] =
          ZIO.succeed(context.DefaultSignatureContext.InstanceParameter(
            decl.instanceName.value,
          ))

        override def signature: Comp[FunctionSignature] = sigCache.get(
          SourceSignature.parse(ctx)(closure.scope)(closure.accessToken)(context.TRExprContext.ExpressionOwner.Method(this))(decl.parameters, decl.returnType)
        )

        override def implementation: Option[Comp[context.implementations.MethodImplementation]] =
          Some(implCache.get(
            for
              sig <- signature
              thisType <- instanceType
              thisParam <- instanceParam
              thisVar = context.TRSignatureContext.instanceVarFromDefault(thisParam).asInstanceVar(
                context.TRExprContext.ExpressionOwner.Method(this),
                context.TRSignatureContext.exprFromDefault(thisType)
              )
              scope2 =  context.Scopes.InstanceVarScope(closure.scope, thisVar)
              scope3 = context.Scopes.ParameterScope(context.TRExprContext.ExpressionOwner.Method(this), scope2, sig.parameters)
              impl <- decl.body match {
                case None =>
                  ZIO.succeed(context.implementations.MethodImplementation.Abstract)

                case Some(ast.FunctionBody.ExternBody(WithLocation(name, loc))) =>
                  closure.externProvider.getExternMethod(name)
                    .flatMap {
                      case Some(ext) =>
                        ZIO.succeed(ctx.implementations.MethodImplementation.Extern(ext))

                      case None =>
                        ErrorLog.logError(CompilerError.UnknownExtern(loc, name))
                          .as(context.implementations.MethodImplementation.Expr(
                            context.DefaultExprContext.Expr.Error()
                          ))
                    }


                case Some(ast.FunctionBody.ExprBody(expr)) =>
                  val tr = new TypeResolver {
                    override val context: ctx.type = ctx
                  }

                  tr.typeCheckExpr(scope3)(expr, sig.returnType, effects, erasure, closure.accessToken)
                    .map(context.implementations.MethodImplementation.Expr.apply)
              }
            yield impl
          ))

        override def toString(): String =
          decl.name.toString()

      },
    )
}
