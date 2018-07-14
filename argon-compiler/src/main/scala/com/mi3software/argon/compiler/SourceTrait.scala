package com.mi3software.argon.compiler

import com.mi3software.argon.compiler.PayloadSpecifiers.DeclarationPayloadSpecifier
import com.mi3software.argon.parser
import com.mi3software.argon.parser.TraitDeclarationStmt
import com.mi3software.argon.util.WithSource
import com.mi3software.argon.Compilation
import scalaz._

private[compiler] object SourceTrait {

  def apply[TComp[+_] : Monad : Compilation]
  (context2: ContextComp[TComp])
  (scope: Scope[context2.ContextScopeTypes])
  (stmt: TraitDeclarationStmt)
  (desc: TraitDescriptor)
  : ArTraitWithPayload[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] =
    new ArTraitWithPayload[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] {
      override val context: context2.type = context2
      override val contextProof: Leibniz[context.type, context2.type, context.type, context2.type] = Leibniz.refl

      override val descriptor: TraitDescriptor = desc

      override val isSealed: Boolean = stmt.modifiers.exists {
        case WithSource(parser.SealedModifier, _) => true
        case _ => false
      }

      override lazy val signature: TComp[Signature[context.typeSystem.type, ArTrait.ResultInfo]] = ??? : TComp[Signature[context.typeSystem.type, ArTrait.ResultInfo]]
      override lazy val methods: TComp[Vector[ArMethodWithPayload[context2.type, DeclarationPayloadSpecifier]]] = ??? : TComp[Vector[ArMethodWithPayload[context2.type, DeclarationPayloadSpecifier]]]
      override lazy val metaType: TComp[MetaClass[ArClassWithPayload[context2.type, DeclarationPayloadSpecifier]]] = ??? : TComp[MetaClass[ArClassWithPayload[context2.type, DeclarationPayloadSpecifier]]]

      override val payload: Unit = ()
    }

}
