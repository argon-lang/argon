package com.mi3software.argon.builder

import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._

import com.mi3software.argon.backend.Backend
import scalaz.effect.IO
import scalaz._
import Scalaz._
import com.mi3software.argon.util.{FileID, FileOperations, FileSpec}
import org.json4s.NoTypeHints
import org.json4s.native.Serialization

final case class BuildInfoFileSpec
(
  backend: String,
  inputFiles: List[String],
  outputFile: String,
  references: List[String],
)

object BuildInfoFileSpec {

  def parseFile(file: File): IO[Option[BuildInfoFileSpec]] =
    FileOperations.fileReader(file) { reader =>
      implicit val formats = Serialization.formats(NoTypeHints)
      IO { Serialization.read[BuildInfoFileSpec](reader).point[Option] }
    }



}

final case class BuildInfo
(
  backend: Backend,
  inputFiles: Vector[FileWithSpec],
  outputFile: File,
  references: Vector[File],
)

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
        backend <- OptionT(Backend.find(spec.backend).point[IO])
        inputFiles <- getFileList(dir)(spec.inputFiles)
        inputFilesWithSpec <- fileListWithSpecs(dir)(inputFiles)
        outputFile <- getOutputFile(dir)(spec.outputFile)
        references <- getFileList(dir)(spec.references)

      } yield BuildInfo(
        backend = backend,
        inputFiles = inputFilesWithSpec,
        outputFile = outputFile,
        references = references,
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
    val pathMatcher = FileSystems.getDefault.getPathMatcher(glob)

    allDirectoryFiles(dir)
      .filter(pathMatcher.matches)
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
