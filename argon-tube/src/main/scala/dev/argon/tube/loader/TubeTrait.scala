package dev.argon.tube.loader

import dev.argon.compiler.*
import dev.argon.tube as t
import dev.argon.util.{*, given}
import zio.*

private[loader] object TubeTrait {
  def apply(ctx: TubeLoader.TubeLoadContext, elemLoader: ElementLoader & HasContext[ctx.type], trt: t.TraitDefinition): ctx.Comp[ArTraitC & HasContext[ctx.type]] =
    for
      enumId <- UniqueIdentifier.make

      specCell <- MemoCell.make[ctx.Env, ctx.Error, ImportSpecifier]
      sigCell <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      methodsCell <- MemoCell.make[ctx.Env, ctx.Error, Seq[ArMethodC & HasContext[ctx.type]]]

    yield new ArTraitC with LoaderUtils with TubeRecordFieldBuilder {

      override val context: ctx.type = ctx
      override protected def elementLoader: ElementLoader & HasContext[context.type] = elemLoader

      override val id: UniqueIdentifier = enumId

      override def importSpecifier: Comp[ImportSpecifier] =
        specCell.get(decodeImportSpecifier(trt.`import`))
        
      override def signature: Comp[FunctionSignature] =
        sigCell.get(decodeFunctionSignature(trt.signature))


      override def methods: Comp[Seq[ArMethod]] =
        methodsCell.get(ZIO.foreach(trt.methods) { method =>
          TubeMethod(ctx, elemLoader, method, MethodOwner.ByTrait(this))
        })
    }
}


