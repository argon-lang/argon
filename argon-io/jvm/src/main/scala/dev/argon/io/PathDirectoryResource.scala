package dev.argon.io

import dev.argon.io.*
import zio.*
import zio.stream.*

import java.io.IOException
import java.nio.file.{Files, Path}

private[io] class PathDirectoryResource(path: Path) extends DirectoryResource[IOException, BinaryResource] {

  override def contents: ZStream[Any, IOException, DirectoryEntry[IOException, BinaryResource]] =
    ZStream.fromJavaStream(Files.list(path).nn)
      .refineToOrDie[IOException]
      .mapZIO { path =>
        ZIO.attempt { Files.isDirectory(path) }.refineToOrDie[IOException].map {
          case true => DirectoryEntry.Subdirectory(path.getFileName.toString, PathDirectoryResource(path))
          case false => DirectoryEntry.File(path.getFileName.toString, PathBinaryResource(path))
        }
      }

  override def fileName: Option[String] = Some(path.toString)
}
