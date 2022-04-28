package dev.argon.expr

import dev.argon.util.UniqueIdentifier
import zio.*
import zio.test.Assertion.*
import zio.test.*
import java.time.Duration
import dev.argon.util.{*, given}

object TypeCheckTests extends ZIOSpecDefault {

  private val fuel = 100

  private val resolver: TestResolver[Any] = new TestResolver[Any]()

  import TestExprContext.*
  import resolver.{traitA, traitB, traitC, traitD, traitE, traitType, genTraitType}

  private def unionType(a: WrapExpr, b: WrapExpr): WrapExpr = WrapExpr.OfExpr(ArExpr(ExprConstructor.UnionType, (a, b)))

  private def intersectionType(a: WrapExpr, b: WrapExpr): WrapExpr =
    WrapExpr.OfExpr(ArExpr(ExprConstructor.IntersectionType, (a, b)))

  private def subTypeOf(a: WrapExpr, b: WrapExpr): WrapExpr =
    WrapExpr.OfExpr(ArExpr(ExprConstructor.SubtypeWitnessType, (a, b)))

  private def resolve(t: WrapExpr): IO[String, resolver.ResolvedImplicit] =
    resolver.tryResolve(t, Map.empty, fuel)
      .flatMap {
        case Some(res) => IO.succeed(res)
        case None => IO.fail("Could not resolve implicit")
      }

  private def checkSubType(a: WrapExpr, b: WrapExpr): UIO[Either[String, resolver.ResolvedImplicit]] =
    resolve(subTypeOf(a, b)).either

  override def spec: ZSpec[Environment & Scope, Any] =
    suite("Type System Tests")(
      suite("Trait Tests")(
        test("forall trait X :: X <: X")(
          check(genTraitType) { x =>
            assertM(checkSubType(x, x))(anything)
          }
        ),
        test("B <: A")(
          assertM(checkSubType(traitType(traitB), traitType(traitA)))(isRight(anything))
        ),
        test("A !<: B")(
          assertM(checkSubType(traitType(traitA), traitType(traitB)))(isLeft(anything))
        ),
        test("C <: B")(
          assertM(checkSubType(traitType(traitC), traitType(traitB)))(isRight(anything))
        ),
        test("B !<: C")(
          assertM(checkSubType(traitType(traitB), traitType(traitC)))(isLeft(anything))
        ),
        test("D <: A")(
          assertM(checkSubType(traitType(traitD), traitType(traitA)))(isRight(anything))
        ),
        test("A !<: D")(
          assertM(checkSubType(traitType(traitA), traitType(traitD)))(isLeft(anything))
        ),
        test("D !<: B")(
          assertM(checkSubType(traitType(traitD), traitType(traitB)))(isLeft(anything))
        ),
        test("B !<: D")(
          assertM(checkSubType(traitType(traitB), traitType(traitD)))(isLeft(anything))
        ),
        test("E <: B")(
          assertM(checkSubType(traitType(traitE), traitType(traitB)))(isRight(anything))
        ),
        test("B !<: E")(
          assertM(checkSubType(traitType(traitB), traitType(traitE)))(isLeft(anything))
        ),
        test("E !<: C")(
          assertM(checkSubType(traitType(traitE), traitType(traitC)))(isLeft(anything))
        ),
        test("C !<: E")(
          assertM(checkSubType(traitType(traitC), traitType(traitE)))(isLeft(anything))
        ),
        test("E <: D")(
          assertM(checkSubType(traitType(traitE), traitType(traitD)))(isRight(anything))
        ),
        test("D !<: E")(
          assertM(checkSubType(traitType(traitD), traitType(traitE)))(isLeft(anything))
        ),
      ),
      suite("Union Type Tests")(
        test("forall trait X, Y :: X <: X | Y")(
          check(genTraitType, genTraitType) { (x, y) =>
            assertM(checkSubType(x, unionType(x, y)))(isRight(anything))
          }
        ),
        test("forall trait X, Y :: X <: Y | X")(
          check(genTraitType, genTraitType) { (x, y) =>
            assertM(checkSubType(x, unionType(y, x)))(isRight(anything))
          }
        ),
        test("forall trait X, Y :: X & Y <: X | Y")(
          check(genTraitType, genTraitType) { (x, y) =>
            assertM(checkSubType(intersectionType(x, y), unionType(x, y)))(isRight(anything))
          }
        ),
      ),
      suite("Intersection Type Tests")(
        test("forall trait X, Y :: X & Y <: X")(
          check(genTraitType, genTraitType) { (x, y) =>
            assertM(checkSubType(intersectionType(x, y), x))(isRight(anything))
          }
        ),
        test("forall trait X, Y :: Y & X <: X")(
          check(genTraitType, genTraitType) { (x, y) =>
            assertM(checkSubType(intersectionType(x, y), x))(isRight(anything))
          }
        ),
      ),
    )

}
