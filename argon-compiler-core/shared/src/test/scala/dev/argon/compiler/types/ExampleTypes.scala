package dev.argon.compiler.types

import dev.argon.compiler.Comp
import dev.argon.compiler.core.{AbsRef, ArModule, ArTrait, GlobalBinding, ModuleId, Namespace, TraitId}
import dev.argon.compiler.core.PayloadSpecifiers.{DeclarationPayloadSpecifier, ReferencePayloadSpecifier}
import dev.argon.compiler.expr.ArExpr.TraitType
import dev.argon.util.{NamespacePath, UniqueIdentifier}
import shapeless.Id
import zio.{IO, UIO}
import zio.stream.Stream
import zio.test.{Gen, Sample}

trait ExampleTypes {

  val context = new DummyContext

  lazy val module = new ArModule[context.type, DeclarationPayloadSpecifier] {
    override val context: ExampleTypes.this.context.type = ExampleTypes.this.context
    override val id: ModuleId = DummyModule.id
    override val globalNamespace: Comp[Namespace[context.type, DeclarationPayloadSpecifier]] =
      IO.die(new UnsupportedOperationException)

    override val referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]] = Vector.empty
  }

  def traitType[TPayloadSpec[_, _]](arTrait: ArTrait[context.type, TPayloadSpec]): TraitType[context.type, Id] =
    TraitType[context.type, Id](AbsRef(arTrait), Vector.empty)

  val traitA: ArTrait[context.type, DeclarationPayloadSpecifier]
  val traitB: ArTrait[context.type, DeclarationPayloadSpecifier]
  val traitC: ArTrait[context.type, DeclarationPayloadSpecifier]
  val traitD: ArTrait[context.type, DeclarationPayloadSpecifier]
  val traitE: ArTrait[context.type, DeclarationPayloadSpecifier]

  def genTraitType: Gen[Any, TraitType[context.type, Id]] = Gen(Stream(traitA, traitB, traitC, traitD, traitE).map { t => Sample(traitType(t), Stream.empty) })
}

object ExampleTypes {

  def make: UIO[ExampleTypes] = for {
    traitAId <- UniqueIdentifier.make
    traitBId <- UniqueIdentifier.make
    traitCId <- UniqueIdentifier.make
    traitDId <- UniqueIdentifier.make
    traitEId <- UniqueIdentifier.make
  } yield new ExampleTypes {
    override val traitA: ArTrait[context.type, DeclarationPayloadSpecifier] = DummyTrait(context)(module, "A", TraitId(traitAId))
    override val traitB: ArTrait[context.type, DeclarationPayloadSpecifier] = DummyTrait(context)(module, "B", TraitId(traitBId), traitType(traitA))
    override val traitC: ArTrait[context.type, DeclarationPayloadSpecifier] = DummyTrait(context)(module, "C", TraitId(traitCId), traitType(traitB))
    override val traitD: ArTrait[context.type, DeclarationPayloadSpecifier] = DummyTrait(context)(module, "D", TraitId(traitDId), traitType(traitA))
    override val traitE: ArTrait[context.type, DeclarationPayloadSpecifier] = DummyTrait(context)(module, "E", TraitId(traitEId), traitType(traitB), traitType(traitD))
  }
}
