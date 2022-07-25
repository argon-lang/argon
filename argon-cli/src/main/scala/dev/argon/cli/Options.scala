package dev.argon.cli

import dev.argon.platform.*

final case class Options
(
  command: Option[Command] = None,
  buildSpec: Option[FilePath] = None,
)


enum Command derives CanEqual {
  case Build
  case Compile
}

