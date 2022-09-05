package dev.argon.io

import java.io.IOException
import zio.*

trait ResourceRecorder {
  def recordBinaryResource[R, E >: IOException](resource: BinaryResource[R, E]): ZIO[R, E, String]
  def recordDirectoryResource[R, E >: IOException](resource: DirectoryResource[R, E, BinaryResource]): ZIO[R, E, String]
}
