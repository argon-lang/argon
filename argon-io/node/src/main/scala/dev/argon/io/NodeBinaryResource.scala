package dev.argon.io

import dev.argon.io.*
import dev.argon.io.jstypes.node.NodeFileSystem
import dev.argon.util.async.AsyncIterableTools
import zio.*
import zio.stream.*

import java.io.IOException
import scala.scalajs.js.JavaScriptException
import scala.scalajs.js.typedarray.{Int8Array, Uint8Array}

import PathUtil.remapIOErrors


private[io] final class NodeBinaryResource(path: String) extends BinaryResource[IOException] {
  override def asBytes: ZStream[Any, IOException, Byte] =
    ZStream.scoped[Any](
      ZIO.acquireRelease(
        ZIO.fromPromiseJS { NodeFileSystem.open(path, "r") }
          .mapError(remapIOErrors)
          .refineToOrDie[IOException]
      )(fh => ZIO.fromPromiseJS(fh.close()).mapError(remapIOErrors).orDie)
    )
      .flatMap { fh =>
        AsyncIterableTools.asyncIterableToZStreamRaw(fh.readableWebStream())
          .map(b => uint8ArrayToChunk(new Uint8Array(b)))
          .flattenChunks
          .mapError(remapIOErrors)
          .refineToOrDie[IOException]
      }

  override def fileName: Option[String] = Some(path)
}
