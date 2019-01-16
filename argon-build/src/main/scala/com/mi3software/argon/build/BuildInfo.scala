package com.mi3software.argon.build

import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._

import scalaz._
import Scalaz._
import scalaz.zio._
import scalaz.zio.interop.scalaz72._
import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.util.{FileID, FileOperations, FileSpec}
import toml.Toml
import toml.Codecs._
import com.mi3software.argon.util.AnyExtensions._

final case class BuildInfoFileSpec
(
  project: ProjectInfoFileSpec,
  compilerOptions: CompilerOptionsFileSpec,
)

final case class ProjectInfoFileSpec
(
  backend: String,
  inputFiles: List[String],
  outputFile: String,
  references: List[String],
)

final case class CompilerOptionsFileSpec
(
  moduleName: String,
)

final case class BuildInfo
(
  backend: Backend,
  inputFiles: Vector[FileWithSpec],
  outputFile: File,
  references: Vector[File],

  compilerOptions: CompilerOptions
)

object BuildInfoFileSpec {

  def parseFile(file: File): IO[Throwable, Option[BuildInfoFileSpec]] =
    FileOperations.readAllText(file)
      .map { text =>
        Toml.parseAs[BuildInfoFileSpec](text).toOption
      }



}

object BuildInfo {

  def loadFile(file: File): IO[Throwable, Option[BuildInfo]] =
    (
      for {
        spec <- OptionT(BuildInfoFileSpec.parseFile(file))
        dir <- OptionT(IO.syncThrowable { file.getParentFile.point[Option] })
        buildInfo <- OptionT(load(dir, spec))
      } yield buildInfo
    ).run


  def load(dir: File, spec: BuildInfoFileSpec): IO[Throwable, Option[BuildInfo]] =
    (
      for {
        backend <- OptionT(IO.point(Backend.find(spec.project.backend)) : IO[Throwable, Option[Backend]])
        inputFiles <- getFileList(dir)(spec.project.inputFiles)
        inputFilesWithSpec <- fileListWithSpecs(dir)(inputFiles)
        outputFile <- getOutputFile(dir)(spec.project.outputFile)
        references <- spec.project.references.toVector.traverseU(refFile => IO.syncThrowable { new File(dir, refFile) }).liftM[OptionT]

      } yield BuildInfo(
        backend = backend,
        inputFiles = inputFilesWithSpec,
        outputFile = outputFile,
        references = references,

        compilerOptions = CompilerOptions(
          moduleDescriptor = ModuleDescriptor(spec.compilerOptions.moduleName)
        )
      )
    ).run

  def getFileList(dir: File)(fileNames: List[String]): OptionT[IO[Throwable, ?], Vector[File]] =
    fileNames
      .toVector
      .traverseM(findMatchingGlobs(dir))
      .liftM[OptionT]

  def allDirectoryFiles(dir: File): Traversable[Path] = new Traversable[Path] {
    override def foreach[U](f: Path => U): Unit = {
      val _ =
        Files.walkFileTree(dir.toPath, new SimpleFileVisitor[Path] {
          override def visitFile(t: Path, basicFileAttributes: BasicFileAttributes): FileVisitResult = {
            val _ = f(t)
            FileVisitResult.CONTINUE
          }
        })
    }
  }

  def findMatchingGlobs(dir: File)(glob: String): IO[Throwable, Vector[File]] = IO.syncException {
    val pathMatcher = FileSystems.getDefault.getPathMatcher("glob:" + glob)

    allDirectoryFiles(dir)
      .filter { path: Path =>
        pathMatcher.matches(dir.toPath.relativize(path))
      }
      .map { _.toFile }
      .toVector
  }

  def fileListWithSpecs(dir: File)(files: Vector[File]): OptionT[IO[Throwable, ?], Vector[FileWithSpec]] =
    files
      .zipWithIndex
      .traverse[IO[Throwable, ?], FileWithSpec] { case (file, id) => fileWithSpec(dir)(id)(file) }
      .liftM[OptionT]

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  def fileWithSpec(dir: File)(id: Int)(file: File): IO[Throwable, FileWithSpec] =
    IO.syncThrowable { dir.toPath.relativize(file.toPath).toString }
      .map { path =>
        FileWithSpec(
          file,
          FileSpec(FileID(id), path)
        )
      }

  def getOutputFile(dir: File)(fileName: String): OptionT[IO[Throwable, ?], File] =
    IO.syncThrowable { new File(dir, fileName) }.upcast[IO[Throwable, File]].liftM[OptionT]

}
