package dev.argon.platform

import dev.argon.io.*

import scala.scalajs.js.JavaScriptException

final class PlatformResourceFactory(baseDir: String) extends ResourceFactory[Any, JavaScriptException] {
  override def directoryResource(name: String): DirectoryResource[Any, JavaScriptException, BinaryResource] =
    NodeDirectoryResource(name)

  override def binaryResource(name: String): BinaryResource[Any, JavaScriptException] =
    NodeBinaryResource(name)
}
