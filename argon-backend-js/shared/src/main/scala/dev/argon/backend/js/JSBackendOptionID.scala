package dev.argon.backend.js

import dev.argon.options.{FileList, OptionID, SingleFile}

sealed trait JSBackendOptionID extends OptionID
object JSBackendOptionID {
  case object Externs extends JSBackendOptionID {
    override type ElementType = FileList
  }

  case object InjectBefore extends JSBackendOptionID {
    override type ElementType = Option[SingleFile]
  }

  case object InjectAfter extends JSBackendOptionID {
    override type ElementType = Option[SingleFile]
  }
}
