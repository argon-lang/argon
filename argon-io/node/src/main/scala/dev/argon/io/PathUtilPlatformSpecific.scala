package dev.argon.io

import dev.argon.io.*
import zio.*
import zio.stream.*

import java.io.IOException
import java.nio.file.Path
import dev.argon.io.jstypes.node.{NodePath, NodeFileSystem}
import dev.argon.util.JSPromiseUtil

import scala.scalajs.js


trait PathUtilPlatformSpecific {
  val live: ZLayer[Any, Nothing, PathUtil] =
    ZLayer.succeed(new PathUtil {
      private def isSystemErrorWithCode(ex: js.Error, code: String): Boolean =
        ex.asInstanceOf[js.Dictionary[Matchable]].get("code").contains(code)

      override def exists(path: String): IO[IOException, Boolean] =
        ZIO.fromPromiseJS(NodeFileSystem.stat(path))
          .as(true)
          .catchSome {
            case js.JavaScriptException(ex) if ex.isInstanceOf[js.Error] && isSystemErrorWithCode(ex.asInstanceOf[js.Error], "ENOENT") =>
              ZIO.succeed(false)
          }
          .refineToOrDie[IOException]

      override def dirname(path: String): UIO[String] =
        ZIO.succeed {
          NodePath.dirname(path)
        }


      override def listDirectory(path: String): Stream[IOException, String] =
        ZStream.fromZIO(ZIO.fromPromiseJS(NodeFileSystem.readdir(path)))
          .flatMap(ZStream.fromIterable)
          .map { entry =>
            NodePath.join(path, entry)
          }
          .refineToOrDie[IOException]


      override def binaryResource(path: String): BinaryResource[Any, IOException] =
        NodeBinaryResource(path)

      override def resourceLayer(path: String): ULayer[ResourceReader & ResourceWriter] =
        ZLayer.succeed(PlatformResourceReader(path)) +!+ ZLayer.succeed(PlatformResourceWriter(path))
    })
}
