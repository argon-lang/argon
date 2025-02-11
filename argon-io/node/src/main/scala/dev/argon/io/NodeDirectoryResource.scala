package dev.argon.io

import dev.argon.io.*
import dev.argon.io.jstypes.node.{NodeFileSystem, NodePath}
import zio.*
import zio.stream.*

import java.io.IOException
import scala.scalajs.js.JavaScriptException

import PathUtil.remapIOErrors

private[io] class NodeDirectoryResource(path: String) extends DirectoryResource[IOException, BinaryResource] {

  override def contents: ZStream[Any, IOException, DirectoryEntry[IOException, BinaryResource]] =
    listDir(path, Seq.empty)

  private def listDir(path: String, nameParts: Seq[String]): ZStream[Any, IOException, DirectoryEntry[IOException, BinaryResource]] =
    ZStream.fromIterableZIO(
      ZIO.fromPromiseJS { NodeFileSystem.readdir(path) }
          .mapBoth(remapIOErrors, _.toSeq)
          .refineToOrDie[IOException]
    )
      .flatMap { entryName =>
        ZStream.unwrap(
          for
            subPath <- ZIO.attempt { NodePath.join(path, entryName) }
              .mapError(remapIOErrors)
              .refineToOrDie[IOException]

            stat <- ZIO.fromPromiseJS { NodeFileSystem.stat(subPath) }
              .mapError(remapIOErrors)
              .refineToOrDie[IOException]
          yield
            if stat.isDirectory() then listDir(subPath, nameParts :+ entryName)
            else ZStream(DirectoryEntry(nameParts, entryName, NodeBinaryResource(path)))
        )
      }


  override def fileName: Option[String] = Some(path)
}
