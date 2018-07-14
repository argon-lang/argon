package com.mi3software.argon.compiler
import scalaz.Leibniz

object InvalidClass {

  def apply[TPayloadSpec[_, _] : InvalidClass.InvalidClassPayload]
  (context2: Context)
  : ArClassWithPayload[context2.type, TPayloadSpec] =
    new ArClassWithPayload[context2.type, TPayloadSpec] {

      override val context: context2.type = context2
      override val contextProof: Leibniz[context2.type, context2.type, context2.type, context2.type] = Leibniz.refl

      override val descriptor: ClassDescriptor = ClassDescriptor.Invalid

      override val isOpen: Boolean = false
      override val isSealed: Boolean = false
      override val isAbstract: Boolean = false

      override val signature: context.Comp[Signature[context.typeSystem.type, ArClass.ResultInfo]] =
      context.compMonadInstance.point(SignatureResult[context.typeSystem.type, ArClass.ResultInfo](
        ArClass.ResultInfo(BaseTypeInfoClass(None, Vector.empty))
      ))

      override val methods: context.Comp[Vector[ArMethodWithPayload[context.type, TPayloadSpec]]] =
      context.compMonadInstance.point(Vector.empty)

      override val classConstructors: context.Comp[Vector[ClassConstructorWithPayload[context.type, TPayloadSpec]]] =
      context.compMonadInstance.point(Vector.empty)

      override val metaType: context.Comp[MetaClass[ArClassWithPayload[context.type, TPayloadSpec]]] =
      context.compMonadInstance.point(MetaClassMetaClass())

      override val payload: TPayloadSpec[Unit, context.TClassMetadata] =
      implicitly[InvalidClass.InvalidClassPayload[TPayloadSpec]].value(context)
    }


  sealed trait InvalidClassPayload[TPayloadSpec[_, _]] {
    def value(context: Context): TPayloadSpec[Unit, context.TClassMetadata]
  }

  implicit def invalidClassReferencePayload: InvalidClassPayload[PayloadSpecifiers.ReferencePayloadSpecifier] = new InvalidClassPayload[PayloadSpecifiers.ReferencePayloadSpecifier] {
    override def value(context: Context): context.TClassMetadata = context.invalidClassMetadata
  }

}
