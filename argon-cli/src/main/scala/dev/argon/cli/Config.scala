package dev.argon.cli

import dev.argon.platform.*

final case class Config
(
  command: Option[Command] = None,
  buildSpec: Option[FilePath] = None,
)


enum Command derives CanEqual {
  case Build
  case Compile
}

