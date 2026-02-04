package dev.argon.tube.loader

import dev.argon.compiler.*
import dev.argon.tube as t
import dev.argon.util.*
import zio.*

private[loader] object TubeTrait {
  def apply(ctx: TubeLoader.TubeLoadContext, elemLoader: ElementLoader & HasContext[ctx.type], trt: t.TraitDefinition): ctx.Comp[ArTraitC & HasContext[ctx.type]] =
    for
      traitId <- UniqueIdentifier.make

      specCell <- MemoCell.make[ctx.Env, ctx.Error, ImportSpecifier]
      sigCell <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      methodsCell <- MemoCell.make[ctx.Env, ctx.Error, Seq[MethodEntry[ctx.type]]]

    yield new ArTraitC with LoaderUtils with TubeRecordFieldBuilder {

      override val context: ctx.type = ctx
      override protected def elementLoader: ElementLoader & HasContext[context.type] = elemLoader

      override val id: UniqueIdentifier = traitId

      override def importSpecifier: Comp[ImportSpecifier] =
        specCell.get(decodeImportSpecifier(trt.`import`))
        
      override def signature: Comp[FunctionSignature] =
        sigCell.get(decodeFunctionSignature(trt.signature))


      override def methods: Comp[Seq[MethodEntry[context.type]]] =
        methodsCell.get(ZIO.foreach(trt.methods) { entry =>
          for
            method <- TubeMethod(ctx, elemLoader, entry.method, MethodOwner.ByTrait(this))
          yield MethodEntry(
            decodeAccessModifier(entry.access),
            method
          )
        })
    }
}


