package dev.argon.compiler.types

import cats._
import cats.implicits._
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr._
import zio.IO
import zio.interop.catz._


sealed trait HoleTypeSystem extends TypeSystem {

  val baseTypeSystem: TypeSystem
  override lazy val context: baseTypeSystem.context.type = baseTypeSystem.context

  override type TTypeWrapper[+A] = HoleType[baseTypeSystem.TTypeWrapper[A]]

  override implicit val typeWrapperInstances: WrapperInstance[TTypeWrapper] = new Traverse[TTypeWrapper] with Monad[TTypeWrapper] {
    override def pure[A](x: A): HoleType[baseTypeSystem.TTypeWrapper[A]] =
      HoleTypeType(baseTypeSystem.typeWrapperInstances.pure(x))

    override def flatMap[A, B](fa: HoleType[baseTypeSystem.TTypeWrapper[A]])(f: A => HoleType[baseTypeSystem.TTypeWrapper[B]]): HoleType[baseTypeSystem.TTypeWrapper[B]] =
      fa match {
        case HoleTypeType(a) => baseTypeSystem.typeWrapperInstances.flatTraverse(a)(f)(implicitly, baseTypeSystem.typeWrapperInstances)
        case HoleTypeHole(id) => HoleTypeHole(id)
      }

    override def tailRecM[A, B](a: A)(f: A => HoleType[baseTypeSystem.TTypeWrapper[Either[A, B]]]): HoleType[baseTypeSystem.TTypeWrapper[B]] =
      f(a) match {
        case HoleTypeType(innerEither) =>
          baseTypeSystem.typeWrapperInstances.sequence(innerEither) match {
            case Left(a) => tailRecM(a)(f)
            case Right(b) => HoleTypeType(b)
          }
        case HoleTypeHole(id) => HoleTypeHole(id)
      }

    override def traverse[G[_]: Applicative, A, B](fa: HoleType[baseTypeSystem.TTypeWrapper[A]])(f: A => G[B]): G[HoleType[baseTypeSystem.TTypeWrapper[B]]] =
      fa match {
        case HoleTypeType(t) => baseTypeSystem.typeWrapperInstances.traverse(t)(f).map(HoleTypeType.apply)
        case HoleTypeHole(id) => HoleTypeHole(id).pure[G].widen
      }

    override def foldLeft[A, B](fa: HoleType[baseTypeSystem.TTypeWrapper[A]], b: B)(f: (B, A) => B): B =
      fa match {
        case HoleTypeType(t) => baseTypeSystem.typeWrapperInstances.foldLeft(t, b)(f)
        case HoleTypeHole(_) => b
      }

    override def foldRight[A, B](fa: HoleType[baseTypeSystem.TTypeWrapper[A]], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa match {
        case HoleTypeType(t) => baseTypeSystem.typeWrapperInstances.foldRight(t, lb)(f)
        case HoleTypeHole(_) => lb
      }
  }

  override def unwrapType[A](t: HoleType[baseTypeSystem.TTypeWrapper[A]]): Option[A] =
    t match {
      case HoleTypeType(t) => baseTypeSystem.unwrapType(t)
      case HoleTypeHole(_) => None
    }

  override def wrapExprType(expr: WrapExpr): Comp[TType] =
    unwrapType(expr) match {
      case Some(t) => getExprType(t)
      case None => IO.succeed(fromSimpleType(TypeOfType[context.type, TTypeWrapper](expr)))
    }

  override def isSubTypeWrapperImpl[A](a: HoleType[baseTypeSystem.TTypeWrapper[A]], b: HoleType[baseTypeSystem.TTypeWrapper[A]]): Comp[Either[(A, A), Option[SubTypeInfo[TTypeWrapper[A]]]]] =
    (a, b) match {
      case (HoleTypeType(aInner), HoleTypeType(bInner)) =>
        baseTypeSystem.isSubTypeWrapperImpl(aInner, bInner).map { _.map { _.map { _.map(HoleTypeType[baseTypeSystem.TTypeWrapper[A]]) } } }

      case (_, _) => IO.succeed(Right(Some(SubTypeInfo(a, b, Vector.empty))))
    }

  override def universeOfWrapExprImpl[A](expr: HoleType[baseTypeSystem.TTypeWrapper[A]]): Comp[Either[A, UniverseExpr]] =
    expr match {
      case HoleTypeHole(_) => IO.succeed(Right(AbstractUniverse()))
      case HoleTypeType(t) => baseTypeSystem.universeOfWrapExprImpl(t)
    }

}

object HoleTypeSystem {

  def apply(ts: TypeSystem): HoleTypeSystem { val baseTypeSystem: ts.type } = new HoleTypeSystem {
    override val baseTypeSystem: ts.type = ts
  }

  def holeTypeConverter
  (context: Context)
  (innerTS: TypeSystem.Aux[context.type])
  (holeTS: TypeSystem.Aux[context.type] {
    type TTypeWrapper[+A] = HoleType[innerTS.TTypeWrapper[A]]
  })
  : TypeSystemConverter.Aux[context.type, innerTS.TTypeWrapper, holeTS.TTypeWrapper] = {
    val context2: context.type = context

    new TypeSystemConverter {

      override val context: context2.type = context2

      override type FromWrap[+A] = innerTS.TTypeWrapper[A]
      override type ToWrap[+A] = holeTS.TTypeWrapper[A]


      override protected implicit val fromWrapInstances: WrapperInstance[innerTS.TTypeWrapper] = innerTS.typeWrapperInstances
      override protected implicit val toWrapInstances: WrapperInstance[holeTS.TTypeWrapper] = holeTS.typeWrapperInstances

      override protected def convertType[A](fromExpr: ArExpr[context.type, FromWrap] => Comp[A])(t: FromWrap[A]): Comp[ToWrap[A]] =
        IO.succeed(HoleTypeType(t))
    }
  }

}