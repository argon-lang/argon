package dev.argon.platform

import dev.argon.io.*
import zio.*
import zio.stream.*

import scala.scalajs.js.JavaScriptException

private[platform] class NodeDirectoryResource(path: String) extends DirectoryResource[Any, JavaScriptException, BinaryResource] {

  override def contents: ZStream[Any, JavaScriptException, DirectoryEntry[Any, JavaScriptException, BinaryResource]] =
    ZStream.fromIterableZIO(
      ZIO.fromPromiseJS { NodeFileSystem.readdir(path) }
        .refineToOrDie[JavaScriptException]
        .map { _.toArray }
    ).mapZIO { entryName =>
      for
        subPath <- ZIO.attempt { NodePath.join(path, entryName) }.refineToOrDie[JavaScriptException]
        stat <- ZIO.fromPromiseJS { NodeFileSystem.stat(subPath) }.refineToOrDie[JavaScriptException]
      yield
        if stat.isDirectory() then DirectoryEntry.Subdirectory(entryName, NodeDirectoryResource(subPath))
        else DirectoryEntry.File(entryName, NodeBinaryResource(subPath))
    }

  override def fileName: Option[String] = Some(path)
}
