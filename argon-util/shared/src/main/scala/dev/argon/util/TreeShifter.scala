package dev.argon.util

import cats.*
import cats.implicits.given

import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

trait TreeShifter[F[_]: Monad] {

  type Shifter[A, B] = TreeShifter.Shifter[F, A, B]

  object StandardShifters {
    given [A]: Shifter[A, A] = Monad[F].pure

    given [A, B](using Shifter[A, B]): Shifter[Seq[A], Seq[B]] with
      override def shift(a: Seq[A]): F[Seq[B]] =
        a.traverse(summon[Shifter[A, B]].shift)
    end given

    given [A, B](using Shifter[A, B]): Shifter[Option[A], Option[B]] with
      override def shift(a: Option[A]): F[Option[B]] =
        a.traverse(summon[Shifter[A, B]].shift)
    end given

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
    new Shifter[A, B] {
      override def shift(a: A): F[B] =
        tupleShifter.shift(
          Tuple.fromProductTyped(
            summonInline[A =:= (A & Product)](a)
          )(using summonInline[Mirror.ProductOf[A] {type MirroredElemTypes = ma.MirroredElemTypes} =:= Mirror.ProductOf[A & Product] {type MirroredElemTypes = ma.MirroredElemTypes}](ma)),
        ).map(mb.fromTuple)
    }
  end autoShifterProduct


  final inline def autoShifterTuple[A <: Tuple, B <: Tuple]: Shifter[A, B] =
    inline erasedValue[A] match {
      case _: (ah *: at) =>
        inline erasedValue[B] match {
          case _: (bh *: bt) =>
            lazy val hShifter = summonInline[Shifter[ah, bh]]
            val tShifter = autoShifterTuple[at, bt]
            summonInline[Shifter[ah *: at, bh *: bt] =:= Shifter[A, B]](
              new Shifter[ah *: at, bh *: bt] {
                override def shift(a: ah *: at): F[bh *: bt] =
                  val (h *: t) = a
                  for
                    h <- hShifter.shift(h)
                    t <- tShifter.shift(t)
                  yield h *: t
                end shift
              }
            )
        }


      case _: EmptyTuple =>
        summonInline[Shifter[EmptyTuple, EmptyTuple] =:= Shifter[A, B]](
          new Shifter[EmptyTuple, EmptyTuple] {
            override def shift(a: EmptyTuple): F[EmptyTuple] =
              Monad[F].pure(a)
          }
        )
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
