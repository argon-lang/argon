package dev.argon.io

import dev.argon.io.*
import zio.*
import zio.stream.*

import java.io.IOException
import java.nio.file.{Path, Files}

object PathUtil {
  def exists(path: Path): IO[IOException, Boolean] =
    ZIO.attemptBlockingIO {
      Files.exists(path)
    }

  def dirname(path: Path): UIO[Path] =
    ZIO.succeed {
      Option(path.toAbsolutePath.nn.getParent)
        .orElse(Option(path.getRoot))
        .get
        .nn
    }

  def listDirectory(path: Path): Stream[IOException, Path] =
    ZStream.fromJavaStream(Files.list(path).nn)
      .refineToOrDie[IOException]

  def binaryResource(path: Path): BinaryResource[IOException] =
    PathBinaryResource(path)


  def directoryResource(path: Path): DirectoryResource[IOException, BinaryResource] =
    PathDirectoryResource(path)



  def writeFile[E >: IOException](path: Path, resource: BinaryResource[E]): IO[E, Unit] =
    ZIO.logTrace(s"Writing file: $path") *>
      ZIO.attempt { Files.createDirectories(path.getParent).nn }.refineToOrDie[IOException] *>
      resource.asBytes.run(ZSink.fromPath(path).foldSink(
        failure = {
          case ex: IOException => ZSink.fail(ex)
          case ex => ZSink.die(ex)
        },
        success = _ => ZSink.succeed(())
      ))

  def writeDir[E >: IOException](path: Path, resource: DirectoryResource[E, BinaryResource]): IO[E, Unit] =
    ZIO.logTrace(s"Writing directory: $path") *>
    ZIO.attempt { Files.createDirectories(path).nn }.refineToOrDie[IOException] *>
      resource.contents.foreach {
        case DirectoryEntry.Subdirectory(name, resource) => writeDir(path.resolve(name).nn, resource)
        case DirectoryEntry.File(name, resource) => writeFile(path.resolve(name).nn, resource)
      }
}
