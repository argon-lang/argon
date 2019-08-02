package dev.argon.compiler.types

import dev.argon.compiler.core.Context
import cats._
import cats.implicits._

object ArTypeSystemConverter {

  def apply
  (context: Context)
  (ts2_outer: TypeSystem[context.type])
  : TypeSystemConverter[context.type, context.typeSystem.type, ts2_outer.type, Id] =
    new TypeSystemConverter[context.type, context.typeSystem.type, ts2_outer.type, Id] {

      override def convertType[A](ts1: context.typeSystem.type)(ts2: ts2_outer.type)(fromExpr: ts2.ArExpr => A)(t: A): ts2.TTypeWrapper[A] =
        ts2.wrapType(t)

    }


}
