package dev.argon.plugins.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.compiler.expr.*
import dev.argon.util.*
import dev.argon.compiler.signature.Signature
import dev.argon.parser
import dev.argon.parser.{TraitDeclarationStmt, IdentifierExpr}
import zio.*

object SourceTrait {

  def make[TOwner]
  (ctx: Context)
  (exprConverter2: ExpressionConverter & HasContext[ctx.type])
  (outerEnv: exprConverter2.Env)
  (traitOwner: TOwner & ArTraitC.Ownership[ctx.type])
  (stmt: TraitDeclarationStmt)
  : ctx.Comp[ArTraitC & HasContext[ctx.type] & HasDeclaration[true] & HasOwner[TOwner]] =
    for {
      traitId <- UniqueIdentifier.make

      innerEnvCell <- MemoCell.make[ctx.Env, ctx.Error, exprConverter2.Env]
      sigCell <-
        MemoCell.make[ctx.Env, ctx.Error,
          (
            Signature[
              ctx.ExprContext.WrapExpr,
              (ctx.ExprContext.WrapExpr, Seq[ctx.ExprContext.ArExpr[ctx.ExprContext.ExprConstructor.TraitType]]),
            ],
            exprConverter2.Env
          )
        ]
      methodsCell <-
        MemoCell.make[ctx.Env, ctx.Error, Map[Option[IdentifierExpr], Seq[ArMethodC
          & HasContext[ctx.type] & HasDeclaration[true] & HasOwner[OwnedByTraitC[ctx.type, traitOwner.type]]]]]
      staticMethodsCell <-
        MemoCell.make[ctx.Env, ctx.Error, Map[Option[IdentifierExpr], Seq[ArMethodC
          & HasContext[ctx.type] & HasDeclaration[true] & HasOwner[OwnedByTraitStaticC[ctx.type, traitOwner.type]]]]]

    } yield new ArTraitC with MethodCreationHelper {
      override val owner: traitOwner.type = traitOwner
      override val context: ctx.type = ctx
      override val id: UniqueIdentifier = traitId

      override def isSealed: Boolean = stmt.modifiers.exists { _.value == parser.SealedModifier }

      override type IsDeclaration = true

      protected override val exprConverter: exprConverter2.type = exprConverter2

      import context.ExprContext.{WrapExpr, ArExpr, ExprConstructor}


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

      override def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasDeclaration[true] & HasOwner[OwnedByTrait[owner.type]]]]] =
        methodsCell.get(buildMethods[OwnedByTrait[owner.type]](OwnedByTraitC.apply)(stmt.instanceBody))

      override def staticMethods
        : Comp[Map[Option[IdentifierExpr], Seq[ArMethod & HasDeclaration[true] & HasOwner[OwnedByTraitStatic[owner.type]]]]] =
        staticMethodsCell.get(buildMethods[OwnedByTraitStatic[owner.type]](OwnedByTraitStaticC.apply)(stmt.body))


      override def validate: Comp[Unit] =
        signature.flatMap { sig =>
          val (_, baseTraits) = sig.unsubstitutedResult

          ZIO.foreachDiscard(baseTraits) { baseTraitType =>
            val baseTrait = baseTraitType.constructor.arTrait
            ZIO.fail(DiagnosticError.SealedTraitExtended(DiagnosticSource.Location(stmt.name.location))).when(baseTrait.isSealed && owner.module.moduleName != baseTrait.owner.module.moduleName)
          }
        }
    }
  end make

}
