package dev.argon.util

import cats.Eq
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline}

object DeriveHelpers {
  inline def eq[A: Mirror.Of]: Eq[A] = EqDerivation.derived

  object EqDerivation {

    def eqSum[T](mirror: Mirror.SumOf[T], elemInstances: => List[Eq[_]]): Eq[T] = new Eq[T] {
      override def eqv(x: T, y: T): Boolean = {
        val ord = mirror.ordinal(x)
        ord == mirror.ordinal(y) && elemInstances(ord).asInstanceOf[Eq[Any]].eqv(x, y)
      }
    }

    def eqProduct[T](mirror: Mirror.ProductOf[T], elemInstances: => List[Eq[_]]): Eq[T] = new Eq[T] {
      override def eqv(x: T, y: T): Boolean = {
        elemInstances.iterator.zip(x.asInstanceOf[Product].productIterator.zip(y.asInstanceOf[Product].productIterator))
          .forall { case (elemInst, (xElem, yElem)) =>
            elemInst.asInstanceOf[Eq[Any]].eqv(xElem, yElem)
          }
      }
    }

    inline def summonElemInstances[T <: Tuple]: List[Eq[_]] =
      inline erasedValue[T] match {
        case _: EmptyTuple => Nil
        case _: (h *: t) => summonInline[Eq[h]] :: summonElemInstances[t]
      }

    inline def derived[T](implicit mirror: Mirror.Of[T]): Eq[T] = {
      lazy val elemInstances = summonElemInstances[mirror.MirroredElemTypes]
      inline mirror match {
        case mirror: Mirror.SumOf[T] => eqSum(mirror, elemInstances)
        case mirror: Mirror.ProductOf[T] => eqProduct(mirror, elemInstances)
      }
    }

  }
}

