package dev.argon.compiler.options

import shapeless.Id

final class OptionInfo[A, I]
(
  val name: String,
  val description: String,
  val codecSelector: CodecSelector[A, I],
  val defaultValueOption: Option[A]
)

object OptionInfo {
  def apply[A, I](name: String, description: String)(implicit codecSelector: CodecSelector[A, I]): OptionInfo[A, I] =
    new OptionInfo(
      name = name,
      description = description,
      codecSelector = codecSelector,
      defaultValueOption = None,
    )

  def apply[A, I](name: String, description: String, defaultValue: A)(implicit codecSelector: CodecSelector[A, I]): OptionInfo[A, I] =
    new OptionInfo(
      name = name,
      description = description,
      codecSelector = codecSelector,
      defaultValueOption = Some(defaultValue)
    )
}
