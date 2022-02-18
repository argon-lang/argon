package dev.argon.compiler.source

import dev.argon.compiler.*
import dev.argon.compiler.definitions.*
import dev.argon.util.*
import dev.argon.compiler.signature.Signature
import dev.argon.parser
import dev.argon.parser.{TraitDeclarationStmt, IdentifierExpr}
import zio.*

object SourceTrait {

  def make[TOwner](ctx: Context)(imports: ctx.Comp[Imports[ctx.type]])(traitOwner: TOwner)(stmt: TraitDeclarationStmt)
    : ctx.Comp[ArTraitC with HasContext[ctx.type] with HasOwner[TOwner]] =
    val imports2 = imports
    for {
      traitId <- UniqueIdentifier.make
      exprConverter2 <- ExpressionConverter.make(ctx)

      outerEnvCell <- MemoCell.make[CompEnv, CompError, exprConverter2.Env]
      innerEnvCell <- MemoCell.make[CompEnv, CompError, exprConverter2.Env]
      sigCell <-
        MemoCell.make[CompEnv, CompError, Signature[
          ctx.ExprContext.WrapExpr,
          (ctx.ExprContext.WrapExpr, Seq[ctx.ExprContext.ArExpr[ctx.ExprContext.ExprConstructor.TraitType]]),
        ]]
      methodsCell <-
        MemoCell.make[CompEnv, CompError, Map[Option[IdentifierExpr], Seq[ArMethodC
          with HasContext[ctx.type] with HasOwner[OwnedByTraitC[ctx.type, traitOwner.type]]]]]
      staticMethodsCell <-
        MemoCell.make[CompEnv, CompError, Map[Option[IdentifierExpr], Seq[ArMethodC
          with HasContext[ctx.type] with HasOwner[OwnedByTraitStaticC[ctx.type, traitOwner.type]]]]]

    } yield new ArTraitC with MethodCreationHelper {
      override val owner: traitOwner.type = traitOwner
      override val context: ctx.type = ctx
      override val id: UniqueIdentifier = traitId

      protected override val exprConverter: exprConverter2.type = exprConverter2

      import context.ExprContext.{WrapExpr, ArExpr, ExprConstructor}

      private def outerEnv: Comp[exprConverter.Env] = EnvHelper.createOuterEnv(exprConverter)(outerEnvCell)(imports)

      override def signature: Comp[Signature[WrapExpr, TraitResult]] =
        sigCell.get(
          outerEnv.flatMap { env =>
            SignatureUtil.create(context)(exprConverter)(env)(stmt.parameters)(
              SignatureUtil.createTraitResult(context)(exprConverter)(stmt)
            )
              .flatMap { (sig, env) =>
                SignatureUtil.resolveHolesSig(context)(exprConverter)(env)(exprConverter.TraitSigHandler)(sig)
                  .map { case (sig, _) => sig: Signature[WrapExpr, TraitResult] }
              }
          }
        )

      override def innerEnv: Comp[exprConverter.Env] =
        EnvHelper.createInnerEnv(exprConverter)(innerEnvCell)(outerEnv)(this)(signature)

      override def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod with HasOwner[OwnedByTrait[owner.type]]]]] =
        methodsCell.get(buildMethods[OwnedByTrait[owner.type]](OwnedByTraitC.apply)(stmt.instanceBody))

      override def staticMethods
        : Comp[Map[Option[IdentifierExpr], Seq[ArMethod with HasOwner[OwnedByTraitStatic[owner.type]]]]] =
        staticMethodsCell.get(buildMethods[OwnedByTraitStatic[owner.type]](OwnedByTraitStaticC.apply)(stmt.body))

    }
  end make

}
