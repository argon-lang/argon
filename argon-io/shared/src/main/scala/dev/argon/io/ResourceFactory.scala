package dev.argon.io

import java.io.IOException

trait ResourceFactory[+E] {
  def directoryResource(name: String): DirectoryResource[E, BinaryResource]
  def binaryResource(name: String): BinaryResource[E]
}
