package dev.argon.plugin.jsapi

import dev.argon.util.AsyncIterableTools.AsyncIterable
import scalajs.js
import scalajs.js.typedarray.Uint8Array

trait ResourceBase extends js.Any {
  val resourceType: "binary" | "directory"
  def fileName: String | Null
}

trait BinaryResource extends ResourceBase {
  override val resourceType: "binary"
  def asAsyncIterable(): AsyncIterable[Uint8Array]
}

sealed trait DirectoryEntry[FileResource <: BinaryResource] extends js.Any {
  val entryType: "subdirectory" | "file"
  val name: String
}

object DirectoryEntry {
  trait Subdirectory[FileResource <: BinaryResource] extends DirectoryEntry[FileResource] {
    override val entryType: "subdirectory"
    val resource: DirectoryResource[FileResource]
  }
  trait File[FileResource <: BinaryResource] extends DirectoryEntry[FileResource] {
    override val entryType: "file"
    val resource: FileResource
  }
}

trait DirectoryResource[FileResource <: BinaryResource] extends ResourceBase {
  override val resourceType: "directory"
  def contents(): AsyncIterable[DirectoryEntry[FileResource]]
}

type FileSystemResource = BinaryResource | DirectoryResource[BinaryResource]

trait ResourceFactory extends js.Any {
  def directoryResource(name: String): DirectoryResource[BinaryResource]
  def binaryResource(name: String): BinaryResource
}

trait ResourceRecorder extends js.Any {
  def recordBinaryResource(resource: BinaryResource): js.Promise[String]
  def recordDirectoryResource(resource: DirectoryResource[BinaryResource]): js.Promise[String]
}
