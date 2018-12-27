package com.mi3software.argon.compiler.loaders.source

import com.mi3software.argon.compiler.PayloadSpecifiers._
import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.compiler.lookup._
import com.mi3software.argon.parser
import com.mi3software.argon.parser.TraitDeclarationStmt
import com.mi3software.argon.util.WithSource
import scalaz._

private[compiler] object SourceTrait {

  def apply[TComp[+_] : Monad : Compilation]
  (context2: ContextComp[TComp])
  (scope: context2.Scope)
  (stmt: TraitDeclarationStmt)
  (desc: TraitDescriptor)
  : ArTrait[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] =
    new ArTrait[context2.type, PayloadSpecifiers.DeclarationPayloadSpecifier] {
      override val context: context2.type = context2
      import context._

      override val contextProof: Leibniz[context.type, context2.type, context.type, context2.type] = Leibniz.refl

      override val descriptor: TraitDescriptor = desc

      override val isSealed: Boolean = stmt.modifiers.exists {
        case WithSource(parser.SealedModifier, _) => true
        case _ => false
      }

      override lazy val signature: TComp[Signature[ArTrait.ResultInfo]] = ??? : TComp[Signature[ArTrait.ResultInfo]]
      override lazy val methods: TComp[Vector[ArMethod[context2.type, DeclarationPayloadSpecifier]]] = ??? : TComp[Vector[ArMethod[context2.type, DeclarationPayloadSpecifier]]]
      override lazy val metaType: TComp[MetaClass[ArClass[context2.type, DeclarationPayloadSpecifier]]] = ??? : TComp[MetaClass[ArClass[context2.type, DeclarationPayloadSpecifier]]]

      override val payload: Unit = ()
    }

}
