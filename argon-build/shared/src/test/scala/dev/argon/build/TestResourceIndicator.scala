package dev.argon.build

import dev.argon.compiler.loaders.ResourceIndicator

sealed trait TestResourceIndicator extends ResourceIndicator
final case class LibraryResourceIndicator(name: String) extends TestResourceIndicator {
  override def extension: String = "armodule"
  override def show: String = name + ".armodule"
}

