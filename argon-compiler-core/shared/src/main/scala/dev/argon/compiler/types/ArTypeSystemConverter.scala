package dev.argon.compiler.types

import dev.argon.compiler.core.Context
import cats.{Id => _, _}
import cats.implicits._
import dev.argon.compiler.Comp
import dev.argon.compiler.expr.{ArExpr, WrapperInstance}
import dev.argon.util.Id
import zio.IO

object ArTypeSystemConverter {

  def apply[Wrap[+_]: WrapperInstance]
  (context2: Context)
  : TypeSystemConverter.Aux[context2.type, context2.typeSystem.TTypeWrapper, Wrap] = {
    new TypeSystemConverter {

      override val context: context2.type = context2
      override type FromWrap[+A] = context2.typeSystem.TTypeWrapper[A]
      override type ToWrap[+A] = Wrap[A]


      override protected val fromWrapInstances: WrapperInstance[Id] = implicitly
      override protected val toWrapInstances: WrapperInstance[Wrap] = implicitly

      override protected def convertType[A](fromExpr: ArExpr[context.type, Id] => Comp[A])(t: A): Comp[Wrap[A]] =
        IO.succeed(t.pure[Wrap])
    }
  }


}
