package dev.argon.platform

import dev.argon.io.{ResourceFactory, ResourceWriter}
import java.io.IOException
import zio.*

trait PathUtil {
  def fromString(path: String): FilePath
  def dirname(path: FilePath): UIO[FilePath]
  def resourceLayer(path: FilePath): ULayer[ResourceFactory & ResourceWriter]
}
