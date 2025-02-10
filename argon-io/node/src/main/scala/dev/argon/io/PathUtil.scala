package dev.argon.io

import dev.argon.io.*
import zio.*
import zio.stream.*

import java.io.{IOException, FileNotFoundException}
import dev.argon.io.jstypes.node.{NodePath, NodeFileSystem}
import dev.argon.util.async.JSPromiseUtil

import scala.scalajs.js
import dev.argon.util.async.TypedArrayUtil
import scala.scalajs.js.JavaScriptException
import java.nio.file.{AccessDeniedException, NotDirectoryException}


object PathUtil {
  private def isSystemErrorWithCode(ex: js.Error, code: String): Boolean =
    ex.asInstanceOf[js.Dictionary[Matchable]].get("code").contains(code)

  private[io] def remapIOErrors(ex: Throwable): Throwable =
    ex match {
      case JavaScriptException(error) if error.isInstanceOf[js.Error] => 
        val error2 = error.asInstanceOf[js.Error]

        ex.asInstanceOf[js.Dictionary[Matchable]].get("code")
          .flatMap { code =>
            if js.typeOf(code) == "string" then
              Some(code.asInstanceOf[String])
            else
              None
          }
          .flatMap(mapErrorByCode(error2).lift)
          .getOrElse(ex)

      case _ => ex
    }

  private def mapErrorByCode(error: js.Error): PartialFunction[String, IOException] = {
    case "ENOENT" => new FileNotFoundException(error.message)
    case "EACCES" => new AccessDeniedException(error.message)
    case "EISDIR" => new IOException(error.message)
    case "ENOTDIR" => new NotDirectoryException(error.message)
    case "EBUSY" => new IOException(error.message)
    case "EEXIST" => new IOException(error.message)
    case "EMFILE" => new IOException(error.message)
    case "ENOSPC" => new IOException(error.message)
    case "EROFS" => new IOException(error.message)
    case "EINVAL"  => new IOException(error.message)
    case "EPERM" => new AccessDeniedException(error.message)
    case "ETIMEDOUT" => new IOException(error.message)
  }

  def exists(path: String): IO[IOException, Boolean] =
    ZIO.fromPromiseJS(NodeFileSystem.stat(path))
      .as(true)
      .catchSome {
        case js.JavaScriptException(ex) if ex.isInstanceOf[js.Error] && isSystemErrorWithCode(ex.asInstanceOf[js.Error], "ENOENT") =>
          ZIO.succeed(false)
      }
      .mapError(remapIOErrors)
      .refineToOrDie[IOException]

  def dirname(path: String): UIO[String] =
    ZIO.succeed {
      NodePath.dirname(path)
    }


  def listDirectory(path: String): Stream[IOException, String] =
    ZStream.unwrap(
      ZIO.fromPromiseJS(NodeFileSystem.readdir(path))
        .mapError(remapIOErrors)
        .refineToOrDie[IOException]
        .map(ZStream.fromIterable)
    )
      .map { entry =>
        NodePath.join(path, entry)
      }


  def binaryResource(path: String): BinaryResource[IOException] =
    NodeBinaryResource(path)

  def directoryResource(name: String): DirectoryResource[IOException, BinaryResource] =
    NodeDirectoryResource(name)

  
  def writeFile[E >: IOException](path: String, resource: BinaryResource[E]): IO[E, Unit] =
    ZIO.acquireReleaseWith(
      ZIO.fromPromiseJS(NodeFileSystem.open(path, "w"))
      .mapError(remapIOErrors)
      .refineToOrDie[IOException]
    )(fh =>
      ZIO.fromPromiseJS(fh.close()).mapError(remapIOErrors).orDie
    ) { fh =>
      resource.asBytes.chunks.map(TypedArrayUtil.fromByteChunk).foreach { data =>
        ZIO.fromPromiseJS(fh.writeFile(data))
          .mapError(remapIOErrors)
          .refineToOrDie[IOException]
      }
    }
  
}
