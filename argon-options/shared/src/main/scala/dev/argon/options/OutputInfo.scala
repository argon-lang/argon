package dev.argon.options

import dev.argon.io.BinaryResource
import java.io.IOException
import zio.*

trait OutputInfo[A[_], Options[_]] {
  val name: String
  val description: String

  def getValue[E](options: Options[E]): A[E]
}


sealed trait OutputInfoAny[Options[_]] {
  type Type[_]
  val info: OutputInfo[Type, Options]
}

object OutputInfoAny {
  def apply[A[_], Options[_]](oi: OutputInfo[A, Options]): OutputInfoAny[Options] =
    new OutputInfoAny[Options] {
      override type Type[E] = A[E]
      override val info: OutputInfo[A, Options] = oi
    }

  given [A[_], Options[_]]: Conversion[OutputInfo[A, Options], OutputInfoAny[Options]] = OutputInfoAny.apply
}

