package com.mi3software.argon.compiler

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FlatSpec, Matchers, PropSpec}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.ScalacheckShapeless._

trait TypeSystemTestsCommon {
  val ts: DummyTypeSystem = new DummyTypeSystem

  protected val dummyTraits: Seq[ts.DummyTrait] = Seq(
    ts.dummyTrait(0, Vector(0)),
    ts.dummyTrait(1, Vector(1, 0)),
    ts.dummyTrait(2, Vector(2, 0)),
    ts.dummyTrait(3, Vector(3, 1, 0)),
  )

  protected val dummyClasses: Seq[ts.DummyClass] = Seq(
    ts.dummyClass(0, Vector(0), Vector(0)),
    ts.dummyClass(1, Vector(1, 0), Vector(1, 0)),
    ts.dummyClass(2, Vector(2, 0), Vector(2, 0)),
    ts.dummyClass(3, Vector(3, 1, 0), Vector(3, 1, 0)),
  )

  protected implicit def traitArb: Arbitrary[ts.DummyTrait] = Arbitrary[ts.DummyTrait](Gen.oneOf(dummyTraits))
  protected implicit def classArb: Arbitrary[ts.DummyClass] = Arbitrary[ts.DummyClass](Gen.oneOf(dummyClasses))

}

class TypeSystemPropTests extends PropSpec with PropertyChecks with Matchers with TypeSystemTestsCommon {



  property("Types should be subtypes of themselves") {
    forAll { (t: ts.TType) =>
      ts.comparer.isSubType(t, t) shouldBe true
    }
  }

}

class TypeSystemTests extends FlatSpec with Matchers with TypeSystemTestsCommon {

  "TypeChecking" should "accept an intersection of a union type" in {

    val t = IntersectionType[ts.type](
      UnionType[ts.type](
        TupleType[ts.type](Vector()),
        TraitType[ts.type](dummyTraits(3))
      ),
      ClassType[ts.type](dummyClasses(1))
    )

    ts.comparer.isSubType(t, t) shouldBe true
  }

  it should "accept a union of an intersection type" in {

    val t = UnionType[ts.type](
      IntersectionType[ts.type](
        TupleType[ts.type](Vector()),
        TraitType[ts.type](dummyTraits(3))
      ),
      ClassType[ts.type](dummyClasses(1))
    )

    ts.comparer.isSubType(t, t) shouldBe true
  }

  it should "accept a union of a union type" in {

    val t = UnionType[ts.type](
      UnionType[ts.type](
        TupleType[ts.type](Vector()),
        TraitType[ts.type](dummyTraits(3))
      ),
      ClassType[ts.type](dummyClasses(1))
    )

    ts.comparer.isSubType(t, t) shouldBe true
  }

  it should "accept an intersection of an intersection type" in {

    val t = IntersectionType[ts.type](
      IntersectionType[ts.type](
        TupleType[ts.type](Vector()),
        TraitType[ts.type](dummyTraits(3))
      ),
      ClassType[ts.type](dummyClasses(1))
    )

    ts.comparer.isSubType(t, t) shouldBe true
  }

}
