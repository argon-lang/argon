package dev.argon.plugin.tube

import dev.argon.io.{BinaryResource, DirectoryResource, FileSystemResource, ResourceRecorder}
import zio.*
import zio.stm.TMap

private[tube] final class MetadataResourceRecorder[R, E] private (val resources: TMap[String, FileSystemResource[R, E]]) extends ResourceRecorder[R, E] {
  private def record(resource: FileSystemResource[R, E]): ZIO[R, E, String] =
    resources.size
      .map(_.toString)
      .tap(resources.put(_, resource))
      .commit

  override def recordBinaryResource(resource: BinaryResource[R, E]): ZIO[R, E, String] =
    record(resource)

  override def recordDirectoryResource(resource: DirectoryResource[R, E, BinaryResource]): ZIO[R, E, String] =
    record(resource)
}

object MetadataResourceRecorder {
  def make[R, E]: UIO[MetadataResourceRecorder[R, E]] =
    for
      resources <- TMap.empty[String, FileSystemResource[R, E]].commit
    yield MetadataResourceRecorder(resources)
}
