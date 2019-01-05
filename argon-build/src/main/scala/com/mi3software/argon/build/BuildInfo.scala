package com.mi3software.argon.build

import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._

import scalaz.effect.IO
import scalaz._
import Scalaz._
import com.mi3software.argon.compiler._
import com.mi3software.argon.compiler.core._
import com.mi3software.argon.util.{FileID, FileOperations, FileSpec}
import toml.Toml
import toml.Codecs._

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

  def parseFile(file: File): IO[Option[BuildInfoFileSpec]] =
    FileOperations.readAllText(file)
      .map { text =>
        Toml.parseAs[BuildInfoFileSpec](text).toOption
      }



}

object BuildInfo {

  def loadFile(file: File): IO[Option[BuildInfo]] =
    (
      for {
        spec <- OptionT(BuildInfoFileSpec.parseFile(file))
        dir <- OptionT(IO { file.getParentFile.point[Option] })
        buildInfo <- OptionT(load(dir, spec))
      } yield buildInfo
    ).run


  def load(dir: File, spec: BuildInfoFileSpec): IO[Option[BuildInfo]] =
    (
      for {
        backend <- OptionT(Backend.find(spec.project.backend).point[IO])
        inputFiles <- getFileList(dir)(spec.project.inputFiles)
        inputFilesWithSpec <- fileListWithSpecs(dir)(inputFiles)
        outputFile <- getOutputFile(dir)(spec.project.outputFile)
        references <- spec.project.references.toVector.traverseU(refFile => IO { new File(dir, refFile) }).liftM[OptionT]

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

  def getFileList(dir: File)(fileNames: List[String]): OptionT[IO, Vector[File]] =
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

  def findMatchingGlobs(dir: File)(glob: String): IO[Vector[File]] = IO {
    val pathMatcher = FileSystems.getDefault.getPathMatcher("glob:" + glob)

    allDirectoryFiles(dir)
      .filter { path: Path =>
        pathMatcher.matches(dir.toPath.relativize(path))
      }
      .map { _.toFile }
      .toVector
  }

  def fileListWithSpecs(dir: File)(files: Vector[File]): OptionT[IO, Vector[FileWithSpec]] =
    files
      .zipWithIndex
      .traverse[IO, FileWithSpec] { case (file, id) => fileWithSpec(dir)(id)(file) }
      .liftM[OptionT]

  @SuppressWarnings(Array("org.wartremover.warts.ToString"))
  def fileWithSpec(dir: File)(id: Int)(file: File): IO[FileWithSpec] =
    IO { dir.toPath.relativize(file.toPath).toString }
      .map { path =>
        FileWithSpec(
          file,
          FileSpec(FileID(id), path)
        )
      }

  def getOutputFile(dir: File)(fileName: String): OptionT[IO, File] =
    IO { new File(dir, fileName) }.liftM[OptionT]

}
