package dev.argon.tube.loader

import dev.argon.ast.IdentifierExpr
import dev.argon.compiler.*
import dev.argon.tube as t
import dev.argon.util.{*, given}
import zio.*

private[loader] object TubeEnum {
  def apply(ctx: Context, elemLoader: ElementLoader & HasContext[ctx.type], e: t.EnumDefinition): ctx.Comp[ArEnumC & HasContext[ctx.type]] =
    for
      enumId <- UniqueIdentifier.make

      specCell <- MemoCell.make[ctx.Env, ctx.Error, ImportSpecifier]
      sigCell <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      variantsCell <- MemoCell.make[ctx.Env, ctx.Error, Seq[EnumVariantC & HasContext[ctx.type]]]

    yield new ArEnumC with LoaderUtils with TubeRecordFieldBuilder {

      override val context: ctx.type = ctx
      override protected def elementLoader: ElementLoader & HasContext[context.type] = elemLoader

      override val id: UniqueIdentifier = enumId

      override def importSpecifier: Comp[ImportSpecifier] =
        specCell.get(decodeImportSpecifier(e.`import`))
        
      override def signature: Comp[FunctionSignature] =
        sigCell.get(decodeFunctionSignature(e.signature))

      override def variants: Comp[Seq[EnumVariant]] =
        variantsCell.get(
          ZIO.foreach(e.variants) { variantDef =>
            val enumInstance = this
            for
              variantId <- UniqueIdentifier.make
              sig <- decodeFunctionSignature(variantDef.signature)
              fieldsCell <- MemoCell.make[ctx.Env, ctx.Error, Seq[RecordFieldC & HasContext[ctx.type]]]
            yield new EnumVariantC {
              override val context: ctx.type = ctx
              override val id: UniqueIdentifier = variantId

              override def owningEnum: ArEnum = enumInstance
              override val name: IdentifierExpr = decodeIdentifier(variantDef.name)

              override def signature: Comp[FunctionSignature] =
                ZIO.succeed(sig)

              override def fields: Comp[Seq[RecordField]] =
                fieldsCell.get(
                  ZIO.foreach(variantDef.fields)(createRecordField(this))
                )
            }
          })
    }
}


