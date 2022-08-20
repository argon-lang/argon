package dev.argon.util

import dev.argon.util

import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror

trait TypeNameTag[T] {
  def typeName(value: T): String
}

object TypeNameTag {

  trait SubTypeNames[T <: Tuple] {
    def value: List[String]
  }

  given SubTypeNames[EmptyTuple] with
    override def value: List[String] = Nil
  end given

  given [H <: String: ValueOf, T <: Tuple: SubTypeNames]: SubTypeNames[H *: T] with
    override def value: List[String] =
      summon[ValueOf[H]].value :: summon[SubTypeNames[T]].value
  end given

  inline def derived[T](using mirror: Mirror.Of[T]): TypeNameTag[T] =
    inline mirror match
      case mirror: Mirror.SumOf[T] =>
        val subTypeNames = summonInline[SubTypeNames[mirror.MirroredElemLabels]].value
        new TypeNameTag[T]:
          override def typeName(value: T): String =
            subTypeNames(mirror.ordinal(value))
        end new

      case mirror: Mirror.ProductOf[T] =>
        val name = summonInline[ValueOf[mirror.MirroredLabel]].value
        new TypeNameTag[T]:
          override def typeName(value: T): String = name
        end new
    end match
}
