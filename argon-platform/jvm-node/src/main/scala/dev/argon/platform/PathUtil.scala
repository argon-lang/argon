package dev.argon.platform

import dev.argon.io.{BinaryResource, ResourceFactory, ResourceWriter}

import java.io.IOException
import zio.*

trait PathUtil {
  def dirname(path: FilePath): UIO[FilePath]
  def binaryResource(path: FilePath): BinaryResource[Any, IOException]
  def resourceLayer(baseDir: FilePath): ULayer[ResourceFactory & ResourceWriter]
}
