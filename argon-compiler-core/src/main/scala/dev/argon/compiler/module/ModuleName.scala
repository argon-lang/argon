package dev.argon.compiler.module

import dev.argon.compiler.tube.*

final case class ModuleName(tubeName: TubeName, path: ModulePath) derives CanEqual {
  override def toString: String = s"$tubeName/$path"
}
