package com.mi3software.argon.compiler

import scalaz._
import Scalaz._

final class DummyTypeSystem extends TypeSystem {
  override type TType = TypeBase[this.type]

  sealed class DummyTrait private[DummyTypeSystem](val id: Int, val baseTraits: Vector[Int]) {
    override def toString: String = s"DummyTrait #$id"
  }
  sealed class DummyClass private[DummyTypeSystem](val id: Int, val baseClasses: Vector[Int], val baseTraits: Vector[Int]) {
    override def toString: String = s"DummyClass #$id"
  }
  sealed case class DummyDataCtor(id: Int, returnType: TType)

  def dummyTrait(id: Int, baseTraits: Vector[Int]): DummyTrait =
    new DummyTrait(id, baseTraits)

  def dummyClass(id: Int, baseClasses: Vector[Int], baseTraits: Vector[Int]): DummyClass =
    new DummyClass(id, baseClasses, baseTraits)

  override type TTraitInfo = DummyTrait
  override type TClassInfo = DummyClass
  override type TDataConstructorInfo = DummyDataCtor

  val comparer: TypeComparer[this.type] = new TypeComparer[this.type] {
    override def isSubTraitInfo(a: DummyTrait, b: DummyTrait): Boolean =
      a.baseTraits contains b.id

    override def isSubClassInfo(a: DummyClass, b: DummyClass): Boolean =
      a.baseClasses contains b.id

    override def classImplementsTrait(c: DummyClass, t: DummyTrait): Boolean =
      c.baseTraits contains t.id

    override def isSameDataConstructorInfo(a: DummyDataCtor, b: DummyDataCtor): Boolean =
      a.id === b.id

    override def dataConstructorReturnType(ctor: DummyDataCtor): TypeBase[DummyTypeSystem.this.type] =
      ctor.returnType

    override def traitMetaClass(traitInfo: DummyTrait): ClassType[DummyTypeSystem.this.type] =
      ClassType[DummyTypeSystem.this.type](dummyClass(-1, Vector(-1), Vector()))

    override def classMetaClass(classInfo: DummyClass): ClassType[DummyTypeSystem.this.type] =
      ClassType[DummyTypeSystem.this.type](dummyClass(-1, Vector(-1), Vector()))

    override def typeBaseToType(typeBase: TypeBase[DummyTypeSystem.this.type]): TypeBase[DummyTypeSystem.this.type] =
      typeBase

    override def isSubType(a: TypeBase[DummyTypeSystem.this.type], b: TypeBase[DummyTypeSystem.this.type]): Boolean =
      isSubTypeBase(a, b)
  }
}


