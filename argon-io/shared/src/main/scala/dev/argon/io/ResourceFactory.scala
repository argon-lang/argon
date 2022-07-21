package dev.argon.io

trait ResourceFactory[R, E] {
  def directoryResource(name: String): DirectoryResource[R, E, BinaryResource]
  def binaryResource(name: String): BinaryResource[R, E]
}
