package com.mi3software.argon.compiler

import scalaz._
import Scalaz._

final class DummyTypeSystem extends TypeSystem {
  override type TType = TypeBase[this.type]

  sealed class DummyTrait(val id: Int, val baseTraits: Vector[Int]) {
    override def toString: String = s"DummyTrait #$id"
  }
  sealed class DummyClass(val id: Int, val baseClasses: Vector[Int], val baseTraits: Vector[Int]) {
    override def toString: String = s"DummyClass #$id"
  }
  sealed class DummyDataCtor(val id: Int, val returnType: TType) {
    override def toString: String = s"DummyDataCtor #$id"
  }

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

    override def traitMetaClass(traitInfo: DummyTrait): ClassType[DummyTypeSystem.this.type] = ???

    override def classMetaClass(classInfo: DummyClass): ClassType[DummyTypeSystem.this.type] = ???

    override def typeBaseToType(typeBase: TypeBase[DummyTypeSystem.this.type]): TypeBase[DummyTypeSystem.this.type] =
      typeBase

    override def isSubType(a: TypeBase[DummyTypeSystem.this.type], b: TypeBase[DummyTypeSystem.this.type]): Boolean =
      isSubTypeBase(a, b)
  }
}


