package dev.argon.compiler.types


import zio.test.{DefaultRunnableSpec, ZSpec}
import zio._
import zio.test._
import zio.test.Assertion._

object TypeSystemTests extends DefaultRunnableSpec {



  override def spec: ZSpec[Environment, Failure] =
    Spec.suite(
      "Type System Tests",
      Managed.fromEffect(
        for {
          moduleEmitter <- ExampleTypes.make
        } yield Vector(traitTests(moduleEmitter))
      ),
      None
    )

  private def traitTests(exampleTypes: ExampleTypes): ZSpec[Environment, Failure] = {
    import exampleTypes._
    import context.typeSystem

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

}
