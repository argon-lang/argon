package dev.argon.tube.loader

import dev.argon.tube as t
import dev.argon.compiler.*
import dev.argon.util.{*, given}
import zio.*

private[loader] object TubeFunction {
  def apply(ctx: TubeLoader.TubeLoadContext, elemLoader: ElementLoader & HasContext[ctx.type], function: t.FunctionDefinition): ctx.Comp[ArFuncC & HasContext[ctx.type]] =
    for
      funcId <- UniqueIdentifier.make

      specCell <- MemoCell.make[ctx.Env, ctx.Error, ImportSpecifier]
      sigCell <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      implCell <- MemoCell.make[ctx.Env, ctx.Error, ctx.implementations.FunctionImplementation]

    yield new ArFuncC with LoaderUtils {

      override val context: ctx.type = ctx
      override protected def elementLoader: ElementLoader & HasContext[context.type] = elemLoader

      override val id: UniqueIdentifier = funcId

      override def isInline: Boolean = function.inline

      override def isErased: Boolean = function.erased

      override def importSpecifier: Comp[ImportSpecifier] =
        specCell.get(decodeImportSpecifier(function.`import`))
        
      override def signature: Comp[FunctionSignature] =
        sigCell.get(decodeFunctionSignature(function.signature))

      override def implementation: Option[Comp[context.implementations.FunctionImplementation]] =
        function.implementation.map { impl =>
          implCell.get(impl match {
            case t.FunctionImplementation.Expr(body) =>
              for
                body <- decodeExpr(body)
              yield context.implementations.FunctionImplementation.Expr(body)

            case t.FunctionImplementation.Extern(externMap) =>
              ZIO.succeed(context.implementations.FunctionImplementation.Extern(externMap))
          })
        }

    }
} 

