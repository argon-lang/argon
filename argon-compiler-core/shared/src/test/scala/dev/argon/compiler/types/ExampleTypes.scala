package dev.argon.compiler.types

import dev.argon.compiler.{Comp, CompStream}
import dev.argon.compiler.core.{AbsRef, ArModule, ArTrait, Context, GlobalBinding, ModuleId, PayloadSpecInfo, TraitId}
import dev.argon.compiler.core.PayloadSpecifiers.{DeclarationPayloadSpecifier, ReferencePayloadSpecifier}
import dev.argon.compiler.expr.ArExpr.TraitType
import dev.argon.compiler.options.{CompilerInput, CompilerOptionID}
import dev.argon.options.{FileList, Options}
import dev.argon.util.{NamespacePath, UniqueIdentifier}
import shapeless.Id
import zio.{IO, UIO}
import zio.stream.Stream
import zio.test.{Gen, Sample}

trait ExampleTypes {

  val context: Context.Aux[DummyBackend]

  lazy val module = new ArModule[context.type, DeclarationPayloadSpecifier] {
    override val context: ExampleTypes.this.context.type = ExampleTypes.this.context
    override val id: ModuleId = DummyModule.id


    override val namespaces: CompStream[NamespacePath] =
      Stream.die(new UnsupportedOperationException)

    override def getNamespace(ns: NamespacePath): CompStream[GlobalBinding.NonNamespace[context.type, DeclarationPayloadSpecifier]] =
      Stream.die(new UnsupportedOperationException)

    override val referencedModules: Vector[ArModule[context.type, ReferencePayloadSpecifier]] = Vector.empty
  }

  def traitType[TPayloadSpec[_, _]: PayloadSpecInfo](arTrait: ArTrait[context.type, TPayloadSpec]): TraitType[context.type, Id] =
    TraitType[context.type, Id](AbsRef(arTrait), Vector.empty)

  val traitA: ArTrait[context.type, DeclarationPayloadSpecifier]
  val traitB: ArTrait[context.type, DeclarationPayloadSpecifier]
  val traitC: ArTrait[context.type, DeclarationPayloadSpecifier]
  val traitD: ArTrait[context.type, DeclarationPayloadSpecifier]
  val traitE: ArTrait[context.type, DeclarationPayloadSpecifier]

  def genTraitType: Gen[Any, TraitType[context.type, Id]] = Gen(Stream(traitA, traitB, traitC, traitD, traitE).map { t => Sample(traitType(t), Stream.empty) })
}

object ExampleTypes {

  def make: UIO[ExampleTypes] = {
    val backend = new DummyBackend
    for {
      ctx <- Context.make(backend)(CompilerInput[Nothing](
        options = Options.fromFunction(new Options.OptionValueFunction[Id, CompilerOptionID] {
          override def apply[E](id: CompilerOptionID { type ElementType = E }): Id[E] = id match {
            case CompilerOptionID.ModuleName => "dummy"
            case CompilerOptionID.InputFiles => new FileList(Seq.empty)
            case CompilerOptionID.References => new FileList(Seq.empty)
          }
        }),

        backendOptions = Options.fromFunction[Id, Nothing](new Options.OptionValueFunction[Id, Nothing] {
          override def apply[E](id: Nothing { type ElementType = E }): Id[E] = id
        })
      ))
      traitAId <- UniqueIdentifier.make
      traitBId <- UniqueIdentifier.make
      traitCId <- UniqueIdentifier.make
      traitDId <- UniqueIdentifier.make
      traitEId <- UniqueIdentifier.make
    } yield new ExampleTypes {
      override val context: Context.Aux[DummyBackend] = ctx
      override val traitA: ArTrait[context.type, DeclarationPayloadSpecifier] = DummyTrait(context)(module, "A", TraitId(traitAId))
      override val traitB: ArTrait[context.type, DeclarationPayloadSpecifier] = DummyTrait(context)(module, "B", TraitId(traitBId), traitType(traitA))
      override val traitC: ArTrait[context.type, DeclarationPayloadSpecifier] = DummyTrait(context)(module, "C", TraitId(traitCId), traitType(traitB))
      override val traitD: ArTrait[context.type, DeclarationPayloadSpecifier] = DummyTrait(context)(module, "D", TraitId(traitDId), traitType(traitA))
      override val traitE: ArTrait[context.type, DeclarationPayloadSpecifier] = DummyTrait(context)(module, "E", TraitId(traitEId), traitType(traitB), traitType(traitD))
    }
  }
}
