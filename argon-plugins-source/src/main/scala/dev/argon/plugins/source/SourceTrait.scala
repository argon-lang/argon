package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.expr.*
import dev.argon.util.*
import dev.argon.compiler.signature.Signature
import dev.argon.compiler.vtable.VTableBuilder
import dev.argon.parser
import dev.argon.parser.{IdentifierExpr, TraitDeclarationStmt}
import zio.*

object SourceTrait {

  def make[TOwner]
  (ctx: Context)
  (exprConverter2: ExpressionConverter & HasContext[ctx.type])
  (vtableBuilder: VTableBuilder[ctx.type])
  (outerEnv: exprConverter2.Env)
  (traitOwner: TOwner & ArTraitC.Ownership[ctx.type])
  (stmt: TraitDeclarationStmt)
  : ctx.Comp[ArTraitC & HasContext[ctx.type] & HasImplementation[true] & HasOwner[TOwner]] =
    for {
      traitId <- UniqueIdentifier.make

      innerEnvCell <- MemoCell.make[ctx.Env, ctx.Error, exprConverter2.Env]
      sigCell <-
        MemoCell.make[ctx.Env, ctx.Error,
          (
            Signature[
              ctx.ExprContext.WrapExpr,
              ctx.ExprContext.TraitResult,
            ],
            exprConverter2.Env
          )
        ]
      methodsCell <-
        MemoCell.make[ctx.Env, ctx.Error, Map[Option[IdentifierExpr], Seq[ArMethodC
          & HasContext[ctx.type] & HasImplementation[true] & HasOwner[OwnedByTraitC[ctx.type, traitOwner.type]]]]]
      staticMethodsCell <-
        MemoCell.make[ctx.Env, ctx.Error, Map[Option[IdentifierExpr], Seq[ArMethodC
          & HasContext[ctx.type] & HasImplementation[true] & HasOwner[OwnedByTraitStaticC[ctx.type, traitOwner.type]]]]]

    } yield new ArTraitC with MethodCreationHelper {
      override val owner: traitOwner.type = traitOwner
      override val context: ctx.type = ctx
      override val id: UniqueIdentifier = traitId

      override def isSealed: Boolean = stmt.modifiers.exists { _.value == parser.SealedModifier }

      override type IsImplementation = true

      protected override val exprConverter: exprConverter2.type = exprConverter2

      import context.ExprContext.{WrapExpr, ArExpr, ExprConstructor, TraitResult}


      private def sigEnv: Comp[(Signature[WrapExpr, TraitResult], exprConverter.Env)] =
        sigCell.get(
          SignatureUtil.create(context)(exprConverter)(this)(outerEnv)(stmt.parameters)(
            SignatureUtil.createTraitResult(context)(exprConverter)(stmt)
          )
        )

      override def signature: Comp[Signature[WrapExpr, TraitResult]] =
        sigEnv.map { _._1 }

      override def innerEnv: Comp[exprConverter.Env] =
        sigEnv.map { _._2 }

      override def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[true] & HasOwner[OwnedByTrait[owner.type]]]]] =
        methodsCell.get(buildMethods[OwnedByTrait[owner.type]](OwnedByTraitC.apply)(stmt.instanceBody))

      override def staticMethods
        : Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasImplementation[true] & HasOwner[OwnedByTraitStatic[owner.type]]]]] =
        staticMethodsCell.get(buildMethods[OwnedByTraitStatic[owner.type]](OwnedByTraitStaticC.apply)(stmt.body))

      override def vtable: Comp[context.VT.VTable] =
        vtableBuilder.fromTrait(this)

      override def validate: Comp[Unit] =
        for
          sig <- signature
          baseTraits <- sig.unsubstitutedResult.baseTraits
          _ <- ZIO.foreachDiscard(baseTraits) { baseTraitType =>
            val baseTrait = baseTraitType.constructor.arTrait
            ZIO.fail(DiagnosticError.SealedTraitExtended(DiagnosticSource.Location(stmt.name.location))).when(baseTrait.isSealed && owner.module.moduleName != baseTrait.owner.module.moduleName)
          }
        yield ()
    }
  end make

}
