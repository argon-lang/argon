package com.mi3software.argon.compiler.types

import com.mi3software.argon.compiler.core.Context
import com.mi3software.argon.compiler.types.ExpandTypeSystemConverter.Expander

object ArTypeSystemConverter {

  def apply[F[_]]
  (context: Context)
  (ts2_outer: TypeSystem[context.type])
  : TypeSystemConverter[context.type, context.typeSystem.type, ts2_outer.type] =
    new TypeSystemConverter[context.type, context.typeSystem.type, ts2_outer.type] {
      override def convertType(ts1: context.typeSystem.type)(ts2: ts2_outer.type)(t: ts2.SimpleType): ts2.TTypeWrapper[ts2.SimpleType] =
        ts2.fromSimpleType(t)
    }


}
