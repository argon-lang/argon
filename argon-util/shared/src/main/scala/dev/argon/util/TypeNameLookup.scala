package dev.argon.util

import dev.argon.util

import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror

trait TypeNameLookup[T] {
  def lookup(name: String): Option[T] = asMap.get(name)
  val asMap: Map[String, T]
}

object TypeNameLookup {

  trait LookupMap[T, U] {
    val asMap: Map[String, T]
  }

  given lookupMapSingleton[T, U <: T](using mirror: Mirror.ProductOf[U], nameValue: ValueOf[mirror.MirroredLabel], singletonValue: ValueOf[U]): LookupMap[T, U] with
    override val asMap: Map[String, T] = Map(
      nameValue.value -> singletonValue.value
    )
  end lookupMapSingleton

  given lookupMapSum[T, U <: T](using mirror: Mirror.SumOf[U]): LookupMap[T, U] with
    override val asMap: Map[String, T] = Map.empty
  end lookupMapSum

  given [T]: LookupMap[T, EmptyTuple] with
    override val asMap: Map[String, T] = Map.empty
  end given

  given[T, Head, Tail <: Tuple](using headMap: LookupMap[T, Head], tailMap: LookupMap[T, Tail]): LookupMap[T, Head *: Tail] with
    override val asMap: Map[String, T] = headMap.asMap ++ tailMap.asMap
  end given

  inline def derived[T](using mirror: Mirror.SumOf[T]): TypeNameLookup[T] =
    val map = summonInline[LookupMap[T, mirror.MirroredElemTypes]].asMap
    new TypeNameLookup[T]:
      override val asMap: Map[String, T] = map
    end new
  end derived

}
