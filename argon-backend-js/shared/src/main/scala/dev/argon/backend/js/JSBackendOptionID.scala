package dev.argon.backend.js

import cats.Id
import dev.argon.options.{FileList, OptionID, SingleFile, TypedOptionID}

sealed trait JSBackendOptionID extends OptionID {
  override type Decoded[A] = A
}
object JSBackendOptionID {
  case object Externs extends TypedOptionID[Id, FileList] with JSBackendOptionID
  case object InjectBefore extends TypedOptionID[Id, Option[SingleFile]] with JSBackendOptionID
  case object InjectAfter extends TypedOptionID[Id, Option[SingleFile]] with JSBackendOptionID
}
