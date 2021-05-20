package dev.argon.armodule.loader

import dev.argon.compiler._
import dev.argon.compiler.loaders.ModuleLoader
import cats.{Id => _}
import cats.implicits._
import zio.{URIO, ZIO}
import dev.argon.compiler.output.ArgonModuleSerialized
import dev.argon.io.fileio.ZipRead
import dev.argon.util.MaybeBlocking

object ArgonModuleLoader {

  def make: URIO[ZipRead with MaybeBlocking, ModuleLoader] =
    for {
      protoBufEnv <- ZIO.environment[MaybeBlocking]
      zipRead <- ZIO.access[ZipRead](_.get)
    } yield new ModuleLoader {
      override val supportedExtensions: Seq[String] = Seq("armodule")

      override def loadResource(fileName: String): CompManaged[Option[ArgonModuleSerialized]] =
        zipRead.openZipFile(fileName).catchAll(Compilation.unwrapThrowableManaged).mapM { zip =>
          ZipModuleSource.tryOpen(zip.catchAll(Compilation.unwrapThrowable)).provide(protoBufEnv)(zio.NeedsEnv)
        }

  }


}
