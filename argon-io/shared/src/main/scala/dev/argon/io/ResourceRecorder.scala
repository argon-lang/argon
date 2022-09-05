package dev.argon.io

import zio.*

trait ResourceRecorder[R, E] {
  def recordBinaryResource(resource: BinaryResource[R, E]): ZIO[R, E, String]
  def recordDirectoryResource(resource: DirectoryResource[R, E, BinaryResource]): ZIO[R, E, String]
}
