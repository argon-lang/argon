package dev.argon.io

import java.io.IOException

trait ResourceFactory {
  def directoryResource(name: String): DirectoryResource[Any, IOException, BinaryResource]
  def binaryResource(name: String): BinaryResource[Any, IOException]
}
