package dev.argon.module

import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.Path

final case class PathResourceIndicator(path: Path) extends ResourceIndicator {
  override def extension: String = path.extension
  override def show: String = path.toString
}
