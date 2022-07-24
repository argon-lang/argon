package dev.argon.platform

import dev.argon.io.*
import zio.*
import zio.stream.*

import java.io.IOException
import scala.scalajs.js.JavaScriptException

private[platform] class NodeDirectoryResource(path: String) extends DirectoryResource[Any, IOException, BinaryResource] {

  override def contents: ZStream[Any, IOException, DirectoryEntry[Any, IOException, BinaryResource]] =
    ZStream.fromIterableZIO(
      ZIO.fromPromiseJS { NodeFileSystem.readdir(path) }
        .refineToOrDie[JavaScriptException].mapBoth(IOException(_), _.toArray)
    ).mapZIO { entryName =>
      (
        for
          subPath <- ZIO.attempt { NodePath.join(path, entryName) }.refineToOrDie[JavaScriptException]
          stat <- ZIO.fromPromiseJS { NodeFileSystem.stat(subPath) }.refineToOrDie[JavaScriptException]
        yield
          if stat.isDirectory() then DirectoryEntry.Subdirectory(entryName, NodeDirectoryResource(subPath))
          else DirectoryEntry.File(entryName, NodeBinaryResource(subPath))
      ).mapError(IOException(_))
    }

  override def fileName: Option[String] = Some(path)
}
