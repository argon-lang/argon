package com.mi3software.argon.compiler

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.ScalacheckShapeless._

class TypeSystemTests extends PropSpec with PropertyChecks with Matchers {

  val ts = new DummyTypeSystem

  property("Types should be subtypes of themselves") {

    val dummyTraits = Seq(
      ts.dummyTrait(0, Vector(0)),
      ts.dummyTrait(1, Vector(1, 0)),
      ts.dummyTrait(2, Vector(2, 0)),
      ts.dummyTrait(3, Vector(3, 1, 0)),
    )

    val dummyClasses = Seq(
      ts.dummyClass(0, Vector(0), Vector(0)),
      ts.dummyClass(1, Vector(1, 0), Vector(1, 0)),
      ts.dummyClass(2, Vector(2, 0), Vector(2, 0)),
      ts.dummyClass(3, Vector(3, 1, 0), Vector(3, 1, 0)),
    )

    implicit def traitArb = Arbitrary[ts.DummyTrait](Gen.oneOf(dummyTraits))
    implicit def classArb = Arbitrary[ts.DummyClass](Gen.oneOf(dummyClasses))

    forAll { (t: ts.TType) =>
      ts.comparer.isSubType(t, t) shouldBe true
    }
  }

}
