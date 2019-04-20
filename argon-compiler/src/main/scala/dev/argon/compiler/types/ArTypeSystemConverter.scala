package dev.argon.compiler.types

import dev.argon.compiler.core.Context
import scalaz._
import Scalaz._

object ArTypeSystemConverter {

  def apply
  (context: Context)
  (ts2_outer: TypeSystem[context.type])
  : TypeSystemConverter[context.type, context.typeSystem.type, ts2_outer.type, Id] =
    new TypeSystemConverter[context.type, context.typeSystem.type, ts2_outer.type, Id] {

      override def convertType[A](ts1: context.typeSystem.type)(ts2: ts2_outer.type)(fromSimpleType: ts2.SimpleType => A)(t: A): ts2.TTypeWrapper[A] =
        ts2.wrapType(t)

    }


}
