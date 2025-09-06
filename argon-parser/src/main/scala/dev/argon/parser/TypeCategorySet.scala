package dev.argon.parser

import scala.compiletime.{erasedValue, summonInline}
import scala.quoted.*

private[parser] final case class TypeCategorySet[T](categories: Set[TokenCategory])

private[parser] object TypeCategorySet {

  inline given [T <: Token] => TypeCategorySet[T] = TypeCategorySet(getTypeCategories[T])

  private inline def getTypeCategories[T <: Token]: Set[TokenCategory] =
    ${ getTypeCategoriesImpl[T] }

  private def getTypeCategoriesImpl[T: Type](using q: Quotes): Expr[Set[TokenCategory]] =
    import q.reflect.*

    def loop(tr: TypeRepr): Expr[Set[TokenCategory]] =
      tr.dealias.simplified match
        case OrType(left, right) =>
          val l = loop(left)
          val r = loop(right)
          '{ $l ++ $r }
        case other =>
          other.asType match {
            case '[a] =>
              '{ Set(getTypeCategory[a & Matchable]) }

            case _ => throw new Exception(s"Unexpected type: $other")
          }

    loop(TypeRepr.of[T])
  end getTypeCategoriesImpl
  
  private inline def getTypeCategory[T <: Matchable]: TokenCategory =
    inline erasedValue[T] match {
      case _: Token.TokenWithCategory[tc] =>
        summonInline[ValueOf[tc]].value
    }

}
