package dev.argon.io

trait ResourceFactory[E] {
  def directoryResource(name: String): DirectoryResource[Any, E, BinaryResource]
  def binaryResource(name: String): BinaryResource[Any, E]
}
