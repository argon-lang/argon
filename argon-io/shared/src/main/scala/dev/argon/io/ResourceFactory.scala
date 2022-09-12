package dev.argon.io

import java.io.IOException

trait ResourceFactory[-R, +E] {
  def directoryResource(name: String): DirectoryResource[R, E, BinaryResource]
  def binaryResource(name: String): BinaryResource[R, E]
}
