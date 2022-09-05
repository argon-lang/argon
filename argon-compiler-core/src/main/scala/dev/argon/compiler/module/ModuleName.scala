package dev.argon.compiler.module

import dev.argon.compiler.tube.*

final case class ModuleName(tubeName: TubeName, path: ModulePath) derives CanEqual {
  override def toString: String =
    if path.ids.isEmpty then
      tubeName.toString
    else
      s"$tubeName/$path"
}
