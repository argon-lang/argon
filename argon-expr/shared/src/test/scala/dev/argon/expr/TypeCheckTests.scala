package dev.argon.expr

import dev.argon.util.UniqueIdentifier
import zio.{URIO, ZIO}
import zio.test.Assertion._
import zio.test._
import java.time.Duration

object TypeCheckTests extends DefaultRunnableSpec {

  private val fuel = 100

  private val resolver: TestResolver[zio.Random] = new TestResolver[zio.Random]()

  import TestExprContext._
  import resolver.{traitA, traitB, traitC, traitD, traitE, traitType, genTraitType}

  private def unionType(a: WrapExpr, b: WrapExpr): WrapExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.UnionType, (a, b)))

  private def intersectionType(a: WrapExpr, b: WrapExpr): WrapExpr =
    WrapExpr.OfExpr(ArExpr(ExprConstructor.IntersectionType, (a, b)))

  private def subTypeOf(a: WrapExpr, b: WrapExpr): WrapExpr =
    WrapExpr.OfExpr(ArExpr(ExprConstructor.SubtypeWitnessType, (a, b)))

  private def resolve(t: WrapExpr): ZIO[zio.Random, String, resolver.ResolvedImplicit] =
    resolver.resolve(t, Map.empty, fuel)

  private def checkSubType(a: WrapExpr, b: WrapExpr): URIO[zio.Random, Either[String, resolver.ResolvedImplicit]] =
    resolve(subTypeOf(a, b)).either

  override def spec: ZSpec[Environment, Failure] =
    suite("Type System Tests")(
      suite("Trait Tests")(
        testM("forall trait X :: X <: X")(
          checkM(genTraitType) { x =>
            assertM(checkSubType(x, x))(anything)
          }
        ),
        testM("B <: A")(
          assertM(checkSubType(traitType(traitB), traitType(traitA)))(isRight(anything))
        ),
        testM("A !<: B")(
          assertM(checkSubType(traitType(traitA), traitType(traitB)))(isLeft(anything))
        ),
        testM("C <: B")(
          assertM(checkSubType(traitType(traitC), traitType(traitB)))(isRight(anything))
        ),
        testM("B !<: C")(
          assertM(checkSubType(traitType(traitB), traitType(traitC)))(isLeft(anything))
        ),
        testM("D <: A")(
          assertM(checkSubType(traitType(traitD), traitType(traitA)))(isRight(anything))
        ),
        testM("A !<: D")(
          assertM(checkSubType(traitType(traitA), traitType(traitD)))(isLeft(anything))
        ),
        testM("D !<: B")(
          assertM(checkSubType(traitType(traitD), traitType(traitB)))(isLeft(anything))
        ),
        testM("B !<: D")(
          assertM(checkSubType(traitType(traitB), traitType(traitD)))(isLeft(anything))
        ),
        testM("E <: B")(
          assertM(checkSubType(traitType(traitE), traitType(traitB)))(isRight(anything))
        ),
        testM("B !<: E")(
          assertM(checkSubType(traitType(traitB), traitType(traitE)))(isLeft(anything))
        ),
        testM("E !<: C")(
          assertM(checkSubType(traitType(traitE), traitType(traitC)))(isLeft(anything))
        ),
        testM("C !<: E")(
          assertM(checkSubType(traitType(traitC), traitType(traitE)))(isLeft(anything))
        ),
        testM("E <: D")(
          assertM(checkSubType(traitType(traitE), traitType(traitD)))(isRight(anything))
        ),
        testM("D !<: E")(
          assertM(checkSubType(traitType(traitD), traitType(traitE)))(isLeft(anything))
        ),
      ),
      suite("Union Type Tests")(
        testM("forall trait X, Y :: X <: X | Y")(
          checkM(genTraitType, genTraitType) { (x, y) =>
            assertM(checkSubType(x, unionType(x, y)))(isRight(anything))
          }
        ),
        testM("forall trait X, Y :: X <: Y | X")(
          checkM(genTraitType, genTraitType) { (x, y) =>
            assertM(checkSubType(x, unionType(y, x)))(isRight(anything))
          }
        ),
        testM("forall trait X, Y :: X & Y <: X | Y")(
          checkM(genTraitType, genTraitType) { (x, y) =>
            assertM(checkSubType(intersectionType(x, y), unionType(x, y)))(isRight(anything))
          }
        ),
      ),
      suite("Intersection Type Tests")(
        testM("forall trait X, Y :: X & Y <: X")(
          checkM(genTraitType, genTraitType) { (x, y) =>
            assertM(checkSubType(intersectionType(x, y), x))(isRight(anything))
          }
        ),
        testM("forall trait X, Y :: Y & X <: X")(
          checkM(genTraitType, genTraitType) { (x, y) =>
            assertM(checkSubType(intersectionType(x, y), x))(isRight(anything))
          }
        ),
      ),
    )

}
