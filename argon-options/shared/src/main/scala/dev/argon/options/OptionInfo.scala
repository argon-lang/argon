package dev.argon.options

import dev.argon.io.BinaryResource
import java.io.IOException
import zio.*

sealed trait OptionInfo[E, A, Options, Builder] {
  val name: String
  val description: String

  
  def getValue(options: Options): A
}

trait OptionInfoValue[E, A, Options, Builder] extends OptionInfo[E, A, Options, Builder] {
  def addOptionValue(prev: Builder, value: String): Either[OptionsError.ParseError, Builder]
}

trait OptionInfoResource[E, A, Options, Builder] extends OptionInfo[E, A, Options, Builder] {
  def addOptionValue(prev: Builder, value: BinaryResource[E]): Either[OptionsError.ParseError, Builder]
}

