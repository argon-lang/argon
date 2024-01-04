package dev.argon.io

import dev.argon.io.*
import zio.*
import zio.stream.*

import java.io.IOException
import java.nio.file.{Files, Path}

private[io] final class PlatformResourceWriter(baseDir: Path) extends ResourceWriter {
  override def write[R, E >: IOException](name: String, resource: BinaryResource[R, E]): ZIO[R, E, Unit] =
    writeFile(baseDir.resolve(name).nn, resource)

  override def write[R, E >: IOException](name: String, resource: DirectoryResource[R, E, BinaryResource]): ZIO[R, E, Unit] =
    writeDir(baseDir.resolve(name).nn, resource)

  private def writeFile[R, E >: IOException](path: Path, resource: BinaryResource[R, E]): ZIO[R, E, Unit] =
    ZIO.logTrace(s"Writing file: $path") *>
    resource.asBytes.run(ZSink.fromPath(path).foldSink(
      failure = {
        case ex: IOException => ZSink.fail(ex)
        case ex => ZSink.die(ex)
      },
      success = _ => ZSink.succeed(())
    ))

  private def writeDir[R, E >: IOException](path: Path, resource: DirectoryResource[R, E, BinaryResource]): ZIO[R, E, Unit] =
    ZIO.logTrace(s"Writing directory: $path") *>
    ZIO.attempt { Files.createDirectories(path).nn }.refineToOrDie[IOException] *>
      resource.contents.foreach {
        case DirectoryEntry.Subdirectory(name, resource) => writeDir(path.resolve(name).nn, resource)
        case DirectoryEntry.File(name, resource) => writeFile(path.resolve(name).nn, resource)
      }
}
