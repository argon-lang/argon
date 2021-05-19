package dev.argon.backend.js

import cats.Id
import dev.argon.options.{FileList, OptionID, OptionIDBase, OptionInfo, SingleFile}

sealed trait JSBackendOptionID extends OptionID {
  override type Decoded[A] = A
}
object JSBackendOptionID {
  case object Externs extends OptionIDBase[Id, FileList] with JSBackendOptionID {
    override val info: OptionInfo[FileList] =
      OptionInfo(
        name = "js.extern",
        description = "JS module defining extern function implementations",
        defaultValue = new FileList(Seq.empty),
      )
  }
  case object InjectBefore extends OptionIDBase[Id, Option[SingleFile]] with JSBackendOptionID {
    override val info: OptionInfo[Option[SingleFile]] =
      OptionInfo(
        name = "js.inject.before",
        description = "JS code injected at the top of the module",
        defaultValue = None,
      )
  }
  case object InjectAfter extends OptionIDBase[Id, Option[SingleFile]] with JSBackendOptionID {
    override val info: OptionInfo[Option[SingleFile]] =
      OptionInfo(
        name = "inject.after",
        description = "JS code injected at the bottom of the module",
        defaultValue = None,
      )
  }
}
