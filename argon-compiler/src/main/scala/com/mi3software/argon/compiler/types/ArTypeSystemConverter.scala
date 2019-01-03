package com.mi3software.argon.compiler.types

import com.mi3software.argon.compiler.core.Context
import com.mi3software.argon.compiler.types.ExpandTypeSystemConverter.Expander

object ArTypeSystemConverter {

  def apply[F[_]]
  (context: Context)
  (ts2_outer: TypeSystem[context.type])
  : TypeSystemConverter[context.type, context.typeSystem.type, ts2_outer.type] =
    new TypeSystemConverter[context.type, context.typeSystem.type, ts2_outer.type] {

      override def convertType[A](ts1: context.typeSystem.type)(ts2: ts2_outer.type)(fromSimpleType: ts2.SimpleType => A)(t: A): ts2.TTypeWrapper[A] =
        ts2.wrapType(t)

      override def convertUniverse(ts1: context.typeSystem.type)(ts2: ts2_outer.type)(universe: Universe): ts2.TUniverse =
        universe match {
          case ValueUniverse => ts2.valueUniverse
          case TypeUniverse(inner) => ts2.nextUniverse(convertUniverse(ts1)(ts2)(inner))
        }

      override def convertTypeUniverse(ts1: context.typeSystem.type)(ts2: ts2_outer.type)(universe: TypeUniverse): ts2.TTypeUniverse =
        ts2.nextUniverse(convertUniverse(ts1)(ts2)(universe.prev))

    }


}
