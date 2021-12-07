package dev.argon.options


final class OptionInfo[A]
(
  val name: String,
  val description: String,
  val defaultValueOption: Option[A]
)

object OptionInfo {
  def apply[A](name: String, description: String): OptionInfo[A] =
    new OptionInfo(
      name = name,
      description = description,
      defaultValueOption = None,
    )

  def apply[A](name: String, description: String, defaultValue: A): OptionInfo[A] =
    new OptionInfo(
      name = name,
      description = description,
      defaultValueOption = Some(defaultValue)
    )
}
