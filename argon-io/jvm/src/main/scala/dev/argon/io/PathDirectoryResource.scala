package dev.argon.io

import dev.argon.io.*
import zio.*
import zio.stream.*

import java.io.IOException
import java.nio.file.{Files, Path}
import cats.data.NonEmptySeq

private[io] class PathDirectoryResource(path: Path) extends DirectoryResource[IOException, BinaryResource] {

  override def contents: ZStream[Any, IOException, DirectoryEntry[IOException, BinaryResource]] =
    listDir(path, Seq.empty)


  private def listDir(path: Path, nameParts: Seq[String]): ZStream[Any, IOException, DirectoryEntry[IOException, BinaryResource]] =
    ZStream.fromJavaStream(Files.list(path).nn)
      .refineToOrDie[IOException]
      .flatMap { path =>
        ZStream.unwrap(
          ZIO.attempt { Files.isDirectory(path) }.refineToOrDie[IOException].map {
            case true => listDir(path, nameParts :+ path.getFileName().toString())
            case false => ZStream(DirectoryEntry(nameParts, path.getFileName().toString(), PathBinaryResource(path)))
          }
        )
      }

  override def fileName: Option[String] = Some(path.toString)
}
