package dev.argon.compiler.types

import cats.evidence.Is
import dev.argon.compiler.Comp
import dev.argon.compiler.core._
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.expr.ArExpr.TraitType
import dev.argon.compiler.expr.BaseTypeInfoTrait
import dev.argon.util.{FileID, NamespacePath}
import shapeless.{Id, Nat}
import zio.IO

object DummyTrait {

  def apply(ctx: DummyContext)(module: ArModule[ctx.type, DeclarationPayloadSpecifier], name: String, traitId: TraitId, baseTraits: TraitType[ctx.type, Id]*): ArTrait[ctx.type, DeclarationPayloadSpecifier] =
    new ArTrait[ctx.type, DeclarationPayloadSpecifier] {
      override val context: ctx.type = ctx
      override val contextProof: Is[context.type, ctx.type] = Is.refl

      override val id: TraitId = traitId
      override val owner: TraitOwner[context.type, DeclarationPayloadSpecifier] =
        TraitOwner.ByNamespace(module, NamespacePath.empty, GlobalName.Normal(name))

      override val fileId: FileID = FileID(0)
      override val isSealed: Boolean = false

      import context.typeSystem.TTypeWrapper
      import context.signatureContext.{SignatureResult}

      override val signature: Comp[context.signatureContext.Signature[ArTrait.ResultInfo, _ <: Nat]] =
        IO.succeed(SignatureResult(ArTrait.ResultInfo(IO.succeed(BaseTypeInfoTrait[ctx.type, TTypeWrapper](baseTraits.toVector)))))

      override val methods: Comp[Vector[MethodBinding[ctx.type, DeclarationPayloadSpecifier]]] = IO.succeed(Vector.empty)
      override val staticMethods: Comp[Vector[MethodBinding[ctx.type, DeclarationPayloadSpecifier]]] = IO.succeed(Vector.empty)
    }

}
