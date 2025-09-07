package dev.argon.tube.loader

import dev.argon.tube as t
import dev.argon.compiler.*
import dev.argon.util.{*, given}
import zio.*
import dev.argon.ast.IdentifierExpr

private[loader] object TubeRecord {
  def apply(ctx: Context, elemLoader: ElementLoader & HasContext[ctx.type], rec: t.RecordDefinition): ctx.Comp[ArRecordC & HasContext[ctx.type]] =
    for
      recId <- UniqueIdentifier.make

      specCell <- MemoCell.make[ctx.Env, ctx.Error, ImportSpecifier]
      sigCell <- MemoCell.make[ctx.Env, ctx.Error, ctx.DefaultSignatureContext.FunctionSignature]
      fieldsCell <- MemoCell.make[ctx.Env, ctx.Error, Seq[RecordFieldC & HasContext[ctx.type]]]

    yield new ArRecordC with LoaderUtils with TubeRecordFieldBuilder {

      override val context: ctx.type = ctx
      override protected def elementLoader: ElementLoader & HasContext[context.type] = elemLoader

      override val id: UniqueIdentifier = recId

      override def importSpecifier: Comp[ImportSpecifier] =
        specCell.get(decodeImportSpecifier(rec.`import`))
        
      override def signature: Comp[FunctionSignature] =
        sigCell.get(decodeFunctionSignature(rec.signature))

      override def fields: Comp[Seq[RecordField]] =
        fieldsCell.get(
          ZIO.foreach(rec.fields)(createRecordField(this))
        )

    }
}


