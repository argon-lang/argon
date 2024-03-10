package dev.argon.io

import dev.argon.io.*
import zio.*
import zio.stream.*

import java.io.IOException
import java.nio.file.{Path, Files}

trait PathUtilPlatformSpecific {
  val live: ZLayer[Any, Nothing, PathUtil] =
    ZLayer.succeed(new PathUtil {
      override def exists(path: Path): IO[IOException, Boolean] =
        ZIO.attemptBlockingIO {
          Files.exists(path)
        }

      override def dirname(path: Path): UIO[Path] =
        ZIO.succeed {
          // asInstanceOf needed
          Option(path.toAbsolutePath.nn.getParent)
            .orElse(Option(path.getRoot))
            .get
            .nn
        }

      override def listDirectory(path: Path): Stream[IOException, Path] =
        ZStream.fromJavaStream(Files.list(path).nn)
          .refineToOrDie[IOException]

      override def binaryResource(path: Path): BinaryResource[IOException] =
        PathBinaryResource(path)

      override def resourceLayer(path: Path): ULayer[ResourceReader & ResourceWriter] =
        ZLayer.succeed(PlatformResourceReader(path)) +!+ ZLayer.succeed(PlatformResourceWriter(path))
    })
}
