package com.mi3software.argon.compiler
import scalaz.Leibniz

object InvalidTrait {

  def apply[TPayloadSpec[_, _] : InvalidTrait.InvalidTraitPayload]
  (context2: Context)
  : ArTraitWithPayload[context2.type, TPayloadSpec] =
    new ArTraitWithPayload[context2.type, TPayloadSpec] {

      override val context: context2.type = context2
      override val contextProof: Leibniz[context2.type, context2.type, context2.type, context2.type] = Leibniz.refl

      override val descriptor: TraitDescriptor = TraitDescriptor.Invalid

      override val isSealed: Boolean = false

      override val signature: context.Comp[Signature[context.typeSystem.type, ArTrait.ResultInfo]] =
        context.compMonadInstance.point(SignatureResult[context.typeSystem.type, ArTrait.ResultInfo](
          ArTrait.ResultInfo(BaseTypeInfoTrait(Vector.empty))
        ))

      override val methods: context.Comp[Vector[ArMethodWithPayload[context.type, TPayloadSpec]]] =
        context.compMonadInstance.point(Vector.empty)

      override val metaType: context.Comp[MetaClass[ArClassWithPayload[context.type, TPayloadSpec]]] =
        context.compMonadInstance.point(MetaClassMetaClass())

      override val payload: TPayloadSpec[Unit, context.TTraitMetadata] =
        implicitly[InvalidTrait.InvalidTraitPayload[TPayloadSpec]].value(context)
    }


  sealed trait InvalidTraitPayload[TPayloadSpec[_, _]] {
    def value(context: Context): TPayloadSpec[Unit, context.TTraitMetadata]
  }

  implicit def invalidCTraitReferencePayload: InvalidTraitPayload[PayloadSpecifiers.ReferencePayloadSpecifier] = new InvalidTraitPayload[PayloadSpecifiers.ReferencePayloadSpecifier] {
    override def value(context: Context): context.TTraitMetadata = context.invalidTraitMetadata
  }

}
