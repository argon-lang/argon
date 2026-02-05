package dev.argon.tube.loader

import dev.argon.compiler.*
import dev.argon.expr.ErasureMode
import dev.argon.tube as t
import dev.argon.util.*
import zio.*

private[loader] object TubeInstance {
  def apply(ctx: TubeLoader.TubeLoadContext, elemLoader: ElementLoader & HasContext[ctx.type], inst: t.InstanceDefinition): ctx.Comp[ArInstanceC & HasContext[ctx.type]] =
    for
      instanceId <- UniqueIdentifier.make

      specCell <- MemoCell.make[ctx.Env, ctx.Error, ImportSpecifier]
      sigCell <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      methodsCell <- MemoCell.make[ctx.Env, ctx.Error, Seq[MethodEntry[ctx.type]]]

    yield new ArInstanceC with LoaderUtils with TubeRecordFieldBuilder {

      override val context: ctx.type = ctx
      override protected def elementLoader: ElementLoader & HasContext[context.type] = elemLoader

      override val id: UniqueIdentifier = instanceId

      override def importSpecifier: Comp[ImportSpecifier] =
        specCell.get(decodeImportSpecifier(inst.`import`))

      override def erasureMode: ErasureMode.Declared =
        decodeErasure(inst.erasure)

      override def signature: Comp[FunctionSignature] =
        sigCell.get(decodeFunctionSignature(inst.signature))


      override def methods: Comp[Seq[MethodEntry[context.type]]] =
        methodsCell.get(ZIO.foreach(inst.methods) { entry =>
          for
            method <- TubeMethod(ctx, elemLoader, entry.method, MethodOwner.ByInstance(this))
          yield MethodEntry(
            decodeAccessModifier(entry.access),
            method
          )
        })
    }
}


