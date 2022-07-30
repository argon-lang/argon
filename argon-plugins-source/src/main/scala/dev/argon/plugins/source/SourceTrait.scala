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
  (exprConverter2: ExpressionConverter with HasContext[ctx.type])
  (outerEnv: exprConverter2.Env)
  (traitOwner: TOwner & ArTraitC.Ownership[ctx.type])
  (stmt: TraitDeclarationStmt)
  : ctx.Comp[ArTraitC with HasContext[ctx.type] with HasDeclaration[true] with HasOwner[TOwner]] =
    for {
      traitId <- UniqueIdentifier.make

      innerEnvCell <- MemoCell.make[ctx.Env, ctx.Error, exprConverter2.Env]
      sigCell <-
        MemoCell.make[ctx.Env, ctx.Error, Signature[
          ctx.ExprContext.WrapExpr,
          (ctx.ExprContext.WrapExpr, Seq[ctx.ExprContext.ArExpr[ctx.ExprContext.ExprConstructor.TraitType]]),
        ]]
      methodsCell <-
        MemoCell.make[ctx.Env, ctx.Error, Map[Option[IdentifierExpr], Seq[ArMethodC
          with HasContext[ctx.type] with HasDeclaration[true] with HasOwner[OwnedByTraitC[ctx.type, traitOwner.type]]]]]
      staticMethodsCell <-
        MemoCell.make[ctx.Env, ctx.Error, Map[Option[IdentifierExpr], Seq[ArMethodC
          with HasContext[ctx.type] with HasDeclaration[true] with HasOwner[OwnedByTraitStaticC[ctx.type, traitOwner.type]]]]]

    } yield new ArTraitC with MethodCreationHelper {
      override val owner: traitOwner.type = traitOwner
      override val context: ctx.type = ctx
      override val id: UniqueIdentifier = traitId

      override type IsDeclaration = true

      protected override val exprConverter: exprConverter2.type = exprConverter2

      import context.ExprContext.{WrapExpr, ArExpr, ExprConstructor}

      override def signature: Comp[Signature[WrapExpr, TraitResult]] =
        sigCell.get(
          SignatureUtil.create(context)(exprConverter)(this)(outerEnv)(stmt.parameters)(
            SignatureUtil.createTraitResult(context)(exprConverter)(stmt)
          )
            .flatMap { (sig, env) =>
              SignatureUtil.resolveHolesSig(context)(exprConverter)(env)(exprConverter.traitSigHandler)(sig)
                .map { case (sig, _) => sig: Signature[WrapExpr, TraitResult] }
            }
        )

      override def innerEnv: Comp[exprConverter.Env] =
        EnvHelper.createInnerEnv(exprConverter)(innerEnvCell)(outerEnv)(this)(signature)

      override def methods: Comp[Map[Option[IdentifierExpr], Seq[ArMethod with HasDeclaration[true] with HasOwner[OwnedByTrait[owner.type]]]]] =
        methodsCell.get(buildMethods[OwnedByTrait[owner.type]](OwnedByTraitC.apply)(stmt.instanceBody))

      override def staticMethods
        : Comp[Map[Option[IdentifierExpr], Seq[ArMethod with HasDeclaration[true] with HasOwner[OwnedByTraitStatic[owner.type]]]]] =
        staticMethodsCell.get(buildMethods[OwnedByTraitStatic[owner.type]](OwnedByTraitStaticC.apply)(stmt.body))

    }
  end make

}
