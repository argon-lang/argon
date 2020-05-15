package dev.argon.compiler.types

import dev.argon.compiler.ErrorList
import dev.argon.compiler.core.PayloadSpecifiers.DeclarationPayloadSpecifier
import dev.argon.compiler.core.{AbsRef, ArTrait}
import dev.argon.compiler.expr.ArExpr.TraitType
import zio.test.{DefaultRunnableSpec, ZSpec}
import shapeless.Id
import zio.Cause
import zio.stream._
import zio.test._
import zio.test.Assertion._

object TypeSystemTests extends DefaultRunnableSpec {

  val context = new DummyContext
  def traitType[TPayloadSpec[_, _]](arTrait: ArTrait[context.type, TPayloadSpec]): TraitType[context.type, Id] =
    TraitType[context.type, Id](AbsRef(arTrait), Vector.empty)

  val traitA = DummyTrait(context)("A", 0)
  val traitB = DummyTrait(context)("B", 0, traitType(traitA))
  val traitC = DummyTrait(context)("C", 0, traitType(traitB))
  val traitD = DummyTrait(context)("D", 0, traitType(traitA))
  val traitE = DummyTrait(context)("E", 0, traitType(traitB), traitType(traitD))



  val genTraitType: Gen[Any, TraitType[context.type, Id]] = Gen(Stream(traitA, traitB, traitC, traitD, traitE).map { t => Sample(traitType(t), Stream.empty) })


  import context.typeSystem


  override def spec: ZSpec[Environment, Failure] =
    suite("Type System Tests")(
      traitTests,
    )

  private def traitTests: ZSpec[Environment, Failure] =
    suite("Trait Tests")(
      testM("forall trait X :: X <: X")(
        checkM(genTraitType) { x =>
          assertM(typeSystem.isSubType(x, x))(isSome(anything))
        }
      ),
      testM("B <: A")(
        assertM(typeSystem.isSubType(traitType(traitA), traitType(traitB)))(isSome(anything))
      ),
      testM("A !<: B")(
        assertM(typeSystem.isSubType(traitType(traitB), traitType(traitA)))(isNone)
      ),

      testM("C <: B")(
        assertM(typeSystem.isSubType(traitType(traitB), traitType(traitC)))(isSome(anything))
      ),
      testM("B !<: C")(
        assertM(typeSystem.isSubType(traitType(traitC), traitType(traitB)))(isNone)
      ),

      testM("D <: A")(
        assertM(typeSystem.isSubType(traitType(traitA), traitType(traitD)))(isSome(anything))
      ),
      testM("A !<: D")(
        assertM(typeSystem.isSubType(traitType(traitD), traitType(traitA)))(isNone)
      ),
      testM("D !<: B")(
        assertM(typeSystem.isSubType(traitType(traitB), traitType(traitD)))(isNone)
      ),
      testM("B !<: D")(
        assertM(typeSystem.isSubType(traitType(traitD), traitType(traitB)))(isNone)
      ),

      testM("E <: B")(
        assertM(typeSystem.isSubType(traitType(traitB), traitType(traitE)))(isSome(anything))
      ),
      testM("B !<: E")(
        assertM(typeSystem.isSubType(traitType(traitE), traitType(traitB)))(isNone)
      ),
      testM("E !<: C")(
        assertM(typeSystem.isSubType(traitType(traitC), traitType(traitE)))(isNone)
      ),
      testM("C !<: E")(
        assertM(typeSystem.isSubType(traitType(traitE), traitType(traitC)))(isNone)
      ),
      testM("E <: D")(
        assertM(typeSystem.isSubType(traitType(traitD), traitType(traitE)))(isSome(anything))
      ),
      testM("D !<: E")(
        assertM(typeSystem.isSubType(traitType(traitE), traitType(traitD)))(isNone)
      ),
    )

}
