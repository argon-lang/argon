package dev.argon.source

import dev.argon.ast
import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.*
import dev.argon.util.{*, given}
import zio.*

private[source] object SourceMethod {
  def make(ctx: Context)(scope: ctx.Scopes.Scope, methodOwner: MethodOwner[ctx.type])(decl: ast.MethodDeclarationStmt)(using externProvider: ExternProvider & HasContext[ctx.type]): ctx.Comp[ArMethodC & HasContext[ctx.type]] =
    for
      funcId <- UniqueIdentifier.make
      sigCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      implCache <- MemoCell.make[ctx.Env, ctx.Error, ctx.implementations.MethodImplementation]
      
      erased = decl.modifiers.exists(_.value == ast.Modifier.Erased)
      
      _ <- ErrorLog.logError(CompilerError.ErasedMustBePure(decl.name.location))
        .whenDiscard(erased && !decl.purity)
      
    yield new ArMethodC {
      override val context: ctx.type = ctx
      override val name: IdentifierExpr = decl.name.value
      override val id: UniqueIdentifier = funcId
      override val owner: MethodOwner[context.type] = methodOwner

      override def isInline: Boolean = decl.modifiers.exists(_.value == ast.Modifier.Inline)
      override def isErased: Boolean = erased
      override def isWitness: Boolean = decl.modifiers.exists(_.value == ast.Modifier.Witness)

      override def effects: context.DefaultExprContext.EffectInfo =
        if decl.purity then context.DefaultExprContext.EffectInfo.Pure
        else context.DefaultExprContext.EffectInfo.Effectful

      override val slot: MethodSlot =
        val isAbstract = decl.body.isEmpty
        val isFinal = decl.modifiers.exists(_.value == ast.Modifier.Final)
        val isOverride = decl.modifiers.exists(_.value == ast.Modifier.Override)
        val isVirtual = decl.modifiers.exists(_.value == ast.Modifier.Virtual)
        if isAbstract && isOverride then MethodSlot.AbstractOverride
        else if isAbstract then MethodSlot.Abstract
        else if isFinal && isOverride then MethodSlot.FinalOverride
        else if isOverride then MethodSlot.Override
        else if isVirtual then MethodSlot.Virtual
        else MethodSlot.Final
      end slot

      override def instanceParam: Comp[context.DefaultSignatureContext.InstanceParameter] =
        ZIO.succeed(context.DefaultSignatureContext.InstanceParameter(
          decl.instanceName.value,
        ))

      override def signature: Comp[FunctionSignature] = sigCache.get(
        SourceSignature.parse(ctx)(scope)(context.TRExprContext.ExpressionOwner.Method(this))(decl.parameters, decl.returnType)
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
            scope2 =  context.Scopes.InstanceVarScope(scope, thisVar)
            scope3 = context.Scopes.ParameterScope(context.TRExprContext.ExpressionOwner.Method(this), scope2, sig.parameters)
            impl <- decl.body match {
              case None =>
                ZIO.succeed(context.implementations.MethodImplementation.Abstract)

              case Some(ast.FunctionBody.ExternBody(WithLocation(name, loc))) =>
                externProvider.getExternMethod(name)
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

                tr.typeCheckExpr(scope3)(expr, sig.returnType, effects, erased = isErased)
                  .map(context.implementations.MethodImplementation.Expr.apply)
            }
          yield impl
        ))

      override def toString(): String =
        decl.name.toString()

    }
}
