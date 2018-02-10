package com.mi3software.argon.compiler

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.ScalacheckShapeless._

class TypeSystemTests extends PropSpec with PropertyChecks with Matchers {

  val ts = new DummyTypeSystem

  property("Types should be subtypes of themselves") {

    val dummyTraits = Seq(
      new ts.DummyTrait(0, Vector(0)),
      new ts.DummyTrait(1, Vector(1, 0)),
      new ts.DummyTrait(2, Vector(2, 0)),
      new ts.DummyTrait(3, Vector(3, 1, 0)),
    )

    val dummyClasses = Seq(
      new ts.DummyClass(0, Vector(0), Vector(0)),
      new ts.DummyClass(1, Vector(1, 0), Vector(1, 0)),
      new ts.DummyClass(2, Vector(2, 0), Vector(2, 0)),
      new ts.DummyClass(3, Vector(3, 1, 0), Vector(3, 1, 0)),
    )

    implicit def traitArb = Arbitrary[ts.DummyTrait](Gen.oneOf(dummyTraits))
    implicit def classArb = Arbitrary[ts.DummyClass](Gen.oneOf(dummyClasses))

    forAll { (t: ts.TType) =>
      ts.comparer.isSubType(t, t) shouldBe true
    }
  }

}
