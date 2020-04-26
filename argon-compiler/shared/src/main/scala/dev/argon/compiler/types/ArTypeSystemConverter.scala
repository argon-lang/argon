package dev.argon.compiler.types

import dev.argon.compiler.core.Context
import cats._
import cats.implicits._
import dev.argon.compiler.Comp
import zio.IO

object ArTypeSystemConverter {

  def apply
  (context2: Context)
  (ts2_outer: TypeSystem[context2.type])
  : TypeSystemConverter.Aux[context2.type, context2.typeSystem.type, ts2_outer.type] = {
    new TypeSystemConverter {

      override val context: context2.type = context2
      override val ts: context.typeSystem.type = context.typeSystem
      override val otherTS: ts2_outer.type = ts2_outer

      override protected def convertType[A](fromExpr: otherTS.ArExpr => A)(t: A): Comp[otherTS.TTypeWrapper[A]] =
        IO.succeed(otherTS.wrapType(t))
    }
  }


}
