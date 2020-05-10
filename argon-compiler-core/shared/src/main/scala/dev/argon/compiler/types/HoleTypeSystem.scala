package dev.argon.compiler.types

import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.expr.ArExpr._
import dev.argon.compiler.expr._
import zio.IO


trait HoleTypeSystem extends TypeSystem {

  override type TTypeWrapper[+A] = HoleType[A]

  override def unwrapType[A](t: HoleType[A]): Option[A] =
    t match {
      case HoleTypeType(t) => Some(t)
      case HoleTypeHole(_) => None
    }

  override def wrapExprType(expr: WrapExpr): Comp[TType] =
    expr match {
      case HoleTypeType(t) => getExprType(t)
      case HoleTypeHole(_) => IO.succeed(fromSimpleType(TypeOfType(expr)))
    }

  override def isSubTypeWrapper(a: TType, b: TType): Comp[Option[SubTypeInfo[TType]]] =
    (a, b) match {
      case (HoleTypeType(aInner), HoleTypeType(bInner)) => isSimpleSubType(aInner, bInner)
      case (_, _) => IO.succeed(Some(SubTypeInfo(a, b, Vector.empty)))
    }

  override def universeOfWrapExpr(expr: WrapExpr): Comp[UniverseExpr] =
    expr match {
      case HoleTypeHole(_) => IO.succeed(AbstractUniverse())
      case HoleTypeType(t) => universeOfExpr(t)
    }

}

object HoleTypeSystem {

  def apply(ctx: Context): HoleTypeSystem { val context: ctx.type } = new HoleTypeSystem {
    override val context: ctx.type = ctx
    override val typeWrapperInstances: WrapperInstance[HoleType] = implicitly
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