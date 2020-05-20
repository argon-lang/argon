package dev.argon.compiler.options

import zio.Ref

trait OptionsField[F[_], I] {
  type FieldType

  val info: OptionInfo[FieldType, I]
  val fieldRef: Ref[F[FieldType]]
}
