package dev.argon.cli

import dev.argon.io.PathLike

final case class Options
(
  command: Option[Command] = None,
  buildSpec: Option[PathLike] = None,
)


enum Command derives CanEqual {
  case Build
  case Compile
}

