package dev.argon.build

import dev.argon.compiler.loaders.ResourceIndicator

final case class LibraryResourceIndicator(name: String) extends ResourceIndicator {
  override def extension: String = "armodule"
  override def show: String = name + ".armodule"
}
