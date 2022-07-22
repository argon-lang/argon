package dev.argon.platform

import dev.argon.io.*
import zio.*
import zio.stream.*

import java.io.IOException
import java.nio.file.{Files, Path}

private[platform] class PathDirectoryResource(path: Path) extends DirectoryResource[Any, IOException, BinaryResource] {

  override def contents: ZStream[Any, IOException, DirectoryEntry[Any, IOException, BinaryResource]] =
    ZStream.fromJavaStream(Files.list(path))
      .refineToOrDie[IOException]
      .mapZIO { path =>
        ZIO.attempt { Files.isDirectory(path) }.refineToOrDie[IOException].map {
          case true => DirectoryEntry.Subdirectory(path.getFileName.toString, PathDirectoryResource(path))
          case false => DirectoryEntry.File(path.getFileName.toString, PathBinaryResource(path))
        }
      }

  override def fileName: Option[String] = Some(path.toString)
}
