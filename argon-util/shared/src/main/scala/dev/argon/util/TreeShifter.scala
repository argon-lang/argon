package dev.argon.util

import cats.*
import cats.implicits.given

import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}
import scala.reflect.TypeTest

trait TreeShifter[F[_]: Monad] {

  type Shifter[A, B] = TreeShifter.Shifter[F, A, B]

  object StandardShifters {
    given Shifter[Boolean, Boolean] = identityShifter
    given Shifter[BigInt, BigInt] = identityShifter
    given Shifter[String, String] = identityShifter
    given Shifter[Int, Int] = identityShifter

    given [A, B] => Shifter[A, B] => Shifter[Seq[A], Seq[B]]:
      override def shift(a: Seq[A]): F[Seq[B]] =
        a.traverse(summon[Shifter[A, B]].shift)
    end given

    given [A, B] => Shifter[A, B] => Shifter[Option[A], Option[B]]:
      override def shift(a: Option[A]): F[Option[B]] =
        a.traverse(summon[Shifter[A, B]].shift)
    end given
  }

  def identityShifter[A, B >: A]: Shifter[A, B] = new Shifter[A, B] {
    override def shift(a: A): F[B] = Monad[F].pure[B](a)
  }

  inline def autoShifter[A <: Matchable, B](using ma: Mirror.Of[A], mb: Mirror.Of[B]): Shifter[A, B] =
    inline ma match {
      case ma: Mirror.SumOf[A] =>
        inline mb match {
          case mb: Mirror.SumOf[B] =>
            summonInline[ma.MirroredElemLabels =:= mb.MirroredElemLabels]
            autoShifterSum[A, B](using ma, mb)
        }

      case ma: Mirror.ProductOf[A] =>
        inline mb match {
          case mb: Mirror.ProductOf[B] =>
            summonInline[ma.MirroredElemLabels =:= mb.MirroredElemLabels]
            autoShifterProduct[A, B](using ma, mb)
        }

    }

  inline def autoShifterSum[A <: Matchable, B](using ma: Mirror.SumOf[A], mb: Mirror.SumOf[B]): Shifter[A, B] =
    ${ TreeShifter.autoShifterSumMacro[F, A, B, ma.MirroredElemTypes, mb.MirroredElemTypes]('this, '{ summon[Monad[F]] }) }

  inline def autoShifterProduct[A, B](using ma: Mirror.ProductOf[A], mb: Mirror.ProductOf[B]): Shifter[A, B] =
    val tupleShifter = autoShifterTuple[ma.MirroredElemTypes, mb.MirroredElemTypes]

    val ma2 = summonInline[Mirror.ProductOf[A] {type MirroredElemTypes = ma.MirroredElemTypes} =:= Mirror.ProductOf[A & Product] {type MirroredElemTypes = ma.MirroredElemTypes}](ma)
    val mb2 = summonInline[Mirror.ProductOf[B] {type MirroredElemTypes = mb.MirroredElemTypes} =:= Mirror.ProductOf[B & Product] {type MirroredElemTypes = mb.MirroredElemTypes}](mb)
    summonInline[Shifter[A & Product, B & Product] =:= Shifter[A, B]](
      ProductShifter[A & Product, B & Product](using ma2, mb2)(tupleShifter)
    )
  end autoShifterProduct


  final inline def autoShifterTuple[A <: Tuple, B <: Tuple]: Shifter[A, B] =
    inline erasedValue[A] match {
      case _: (ah *: at) =>
        inline erasedValue[B] match {
          case _: (bh *: bt) =>
            lazy val hShifter = summonInline[Shifter[ah, bh]]
            val tShifter = autoShifterTuple[at, bt]
            summonInline[Shifter[ah *: at, bh *: bt] =:= Shifter[A, B]](
              ConsTupleShifter(hShifter, tShifter)
            )
        }


      case _: EmptyTuple =>
        summonInline[Shifter[EmptyTuple, EmptyTuple] =:= Shifter[A, B]](EmptyTupleShifter)
    }

  final class ProductShifter[A <: Product, B <: Product](using ma: Mirror.ProductOf[A], mb: Mirror.ProductOf[B])(tupleShifter: Shifter[ma.MirroredElemTypes, mb.MirroredElemTypes]) extends Shifter[A, B] {
      override def shift(a: A): F[B] =
        tupleShifter.shift(
          Tuple.fromProductTyped(a)(using ma),
        ).map(mb.fromTuple)
  }

  final class ConsTupleShifter[AH, AT <: Tuple, BH, BT <: Tuple](hShifter: => Shifter[AH, BH], tShifter: Shifter[AT, BT]) extends Shifter[AH *: AT, BH *: BT] {
    override def shift(a: AH *: AT): F[BH *: BT] =
      val (h *: t) = a
      for
        h <- hShifter.shift(h)
        t <- tShifter.shift(t)
      yield h *: t
    end shift
  }
  
  object EmptyTupleShifter extends Shifter[EmptyTuple, EmptyTuple] {
    override def shift(a: EmptyTuple): F[EmptyTuple] =
      Monad[F].pure(a)
  }

}

object TreeShifter {
  @FunctionalInterface
  trait Shifter[F[_], A, B] {
    def shift(a: A): F[B]
  }



  def autoShifterSumMacro[F[_]: Type, A <: Matchable: Type, B: Type, CasesA <: Tuple : Type, CasesB <: Tuple : Type](ts: Expr[TreeShifter[F]], fMonad: Expr[Monad[F]])(using q: Quotes): Expr[Shifter[F, A, B]] =
    '{
      new Shifter[F, A, B] {
        override def shift(a: A): F[B] =
          ${ autoShifterPatternMatch[F, A, B, CasesA, CasesB](ts, 'a, fMonad) }
      }
    }

  def autoShifterPatternMatch[F[_]: Type, A <: Matchable: Type, B: Type, CasesA <: Tuple : Type, CasesB <: Tuple : Type]
  (ts: Expr[TreeShifter[F]], a: Expr[A], fMonad: Expr[Monad[F]])(using q: Quotes): Expr[F[B]] =
    import q.reflect.{*, given}
    val cases = MacroUtils.tupleForeachPair[CasesA, CasesB, CaseDef]([U1, U2] => (u1Type: Type[U1], u2Type: Type[U2]) => {
      given Type[U1] = u1Type
      given Type[U2] = u2Type

      val sym = Symbol.newBind(Symbol.spliceOwner, "v", Flags.EmptyFlags, TypeRepr.of[U1])
      val varExpr = Ref(sym).asExprOf[U1]

      val shiftedValue: Expr[F[B]] =
        Implicits.search(TypeRepr.of[Shifter[F, U1, U2]]) match {
          case searchRes: ImplicitSearchSuccess =>
            '{ $fMonad.map(${searchRes.tree.asExprOf[Shifter[F, U1, U2]]}.shift($varExpr))(summonInline[U2 <:< B]) }

          case _ =>
            Implicits.search(TypeRepr.of[Shifter[F, U1, B]]) match {
              case searchRes: ImplicitSearchSuccess =>
                '{ ${searchRes.tree.asExprOf[Shifter[F, U1, B]]}.shift($varExpr) }

              case _ =>
                '{
                  $fMonad.map(
                    $ts.autoShifterProduct[U1, U2](using summonInline[Mirror.ProductOf[U1]], summonInline[Mirror.ProductOf[U2]])
                      .shift($varExpr)
                  )(summonInline[U2 <:< B])
                }

            }
        }


      CaseDef(
        Bind(sym, Typed(Wildcard(), TypeTree.of[U1])),
        None,
        shiftedValue.asTerm
      )
    })

    Match(a.asTerm, cases).asExprOf[F[B]]
  end autoShifterPatternMatch

}
