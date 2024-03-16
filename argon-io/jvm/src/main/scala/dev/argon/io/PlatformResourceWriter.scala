package dev.argon.io

import dev.argon.io.*
import zio.*
import zio.stream.*

import java.io.IOException
import java.nio.file.{Files, Path}

private[io] final class PlatformResourceWriter(baseDir: Path) extends ResourceWriter {
  override def write[E >: IOException](name: String, resource: BinaryResource[E]): ZIO[Any, E, Unit] =
    writeFile(baseDir.resolve(name).nn, resource)

  override def write[E >: IOException](name: String, resource: DirectoryResource[E, BinaryResource]): ZIO[Any, E, Unit] =
    writeDir(baseDir.resolve(name).nn, resource)

  private def writeFile[E >: IOException](path: Path, resource: BinaryResource[E]): ZIO[Any, E, Unit] =
    ZIO.logTrace(s"Writing file: $path") *>
      ZIO.attempt { Files.createDirectories(path.getParent).nn }.refineToOrDie[IOException] *>
      resource.asBytes.run(ZSink.fromPath(path).foldSink(
        failure = {
          case ex: IOException => ZSink.fail(ex)
          case ex => ZSink.die(ex)
        },
        success = _ => ZSink.succeed(())
      ))

  private def writeDir[E >: IOException](path: Path, resource: DirectoryResource[E, BinaryResource]): ZIO[Any, E, Unit] =
    ZIO.logTrace(s"Writing directory: $path") *>
    ZIO.attempt { Files.createDirectories(path).nn }.refineToOrDie[IOException] *>
      resource.contents.foreach {
        case DirectoryEntry.Subdirectory(name, resource) => writeDir(path.resolve(name).nn, resource)
        case DirectoryEntry.File(name, resource) => writeFile(path.resolve(name).nn, resource)
      }
}
