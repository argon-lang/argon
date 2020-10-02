package dev.argon.backend

import java.io.IOException

import dev.argon.compiler.loaders.{ResourceIndicator, ResourceReader}
import dev.argon.io.Path.PathExtensions
import cats.implicits._
import dev.argon.compiler.{Comp, Compilation, CompilationError}
import dev.argon.compiler.options.OptionsFileHandler
import dev.argon.io.{FilenameManip, Path, ZipFileReader}
import dev.argon.io.fileio.{FileIO, FileIOLite}
import zio._
import zio.stream.ZStream

final case class PathResourceIndicator[P: Path](path: P) extends ResourceIndicator {
  override def extension: String = path.extension
  override def show: String = path.show
}

object PathResourceIndicator {



  def fileHandlerPath[P : Path: Tag](dir: P): OptionsFileHandler[FileIO[P], IOException, String, PathResourceIndicator[P]] = new OptionsFileHandler[FileIO[P], IOException, String, PathResourceIndicator[P]] {

    override def loadSingleFile(file: String): ZIO[FileIO[P], IOException, PathResourceIndicator[P]] =
      Path.of[P](file).map(dir.resolve).map(PathResourceIndicator(_))

  }

  def pathResourceReader[P: Path: Tag]: ZLayer[FileIO[P] with FileIOLite, Nothing, ResourceReader[PathResourceIndicator[P]]] =
    ZLayer.fromFunction { prevLayer =>
      new ResourceReader.ServiceCommon[PathResourceIndicator[P]] with PathResourceIndicatorPlatformSpecific.ReaderService[P] {
        protected val fileIO = prevLayer.get[FileIO.Service[P]]
        override protected val fileIOLite: FileIOLite.Service = prevLayer.get[FileIOLite.Service]


        override def readFile(id: PathResourceIndicator[P]): stream.Stream[CompilationError, Byte] =
          fileIO.readFile(Compilation.unwrapThrowableCause)(id.path)

        override def readTextFile(id: PathResourceIndicator[P]): stream.Stream[CompilationError, Char] =
          fileIO.readText(Compilation.unwrapThrowableCause)(id.path)

        override def readTextFileAsString(id: PathResourceIndicator[P]): Comp[String] =
          fileIO.readAllText(id.path).catchAll { ex => IO.halt(Compilation.unwrapThrowableCause(ex)) }

        override def getZipReader(id: PathResourceIndicator[P]): Managed[CompilationError, ZipFileReader[Any, CompilationError]] =
          fileIO.openZipFile(Compilation.unwrapThrowableCause)(id.path)
      }
    }

}
