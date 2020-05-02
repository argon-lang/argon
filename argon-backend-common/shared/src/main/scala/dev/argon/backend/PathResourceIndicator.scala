package dev.argon.module

import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.Path.PathExtensions
import cats.implicits._
import dev.argon.io.Path

final case class PathResourceIndicator[P: Path](path: P) extends ResourceIndicator {
  override def extension: String = path.extension
  override def show: String = path.show
}
