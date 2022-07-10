package dev.argon.options

import dev.argon.io.{BinaryResource, ResourceDecodeException}

import java.io.IOException
import zio.*
import dev.argon.util.*

sealed trait OptionInfo[A[_], Options[_[_], _]] {
  val name: String
  val description: String

  def getValue[F[_], E](options: Options[F, E]): F[A[E]]
}

trait OptionInfoValue[A, Options[_[_], _]] extends OptionInfo[[_] =>> A, Options] {
  def addOptionValue[E](prev: Options[Option, E], value: String): Either[OptionsError.ParseError, Options[Option, E]]
}

trait OptionInfoResource[A[_], Options[_[_], _]] extends OptionInfo[A, Options] {
  def addOptionValue[E >: ResourceDecodeException | IOException](prev: Options[Option, E], value: BinaryResource[E]): Either[OptionsError.ParseError, Options[Option, E]]
}


sealed trait OptionInfoAny[Options[_[_], _]] {
  type Type[_]
  val info: OptionInfo[Type, Options]
}

object OptionInfoAny {
  def apply[A[_], Options[_[_], _]](oi: OptionInfo[A, Options]): OptionInfoAny[Options] =
    new OptionInfoAny[Options] {
      override type Type[E] = A[E]
      override val info: OptionInfo[A, Options] = oi
    }

  given [A[_], Options[_[_], _]]: Conversion[OptionInfo[A, Options], OptionInfoAny[Options]] = OptionInfoAny.apply
}

