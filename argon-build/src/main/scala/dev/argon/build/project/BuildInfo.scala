package dev.argon.build.project

import java.io.File
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._

import scalaz._
import Scalaz._
import dev.argon.build.Backend
import scalaz.zio._
import scalaz.zio.interop.scalaz72._
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.util.FileOperations
import toml.Toml
import toml.Codecs._
import dev.argon.util.AnyExtensions._
import org.apache.commons.io.FilenameUtils
import shapeless.{Id => _, _}

final case class ProjectInfoFormat[F[_], I]
(
  inputFiles: F[List[I]],
  references: F[List[I]],
)

trait BuildInfo[I] {
  val backend: Backend
  val project: ProjectInfoFormat[Id, I]
  val compilerOptions: CompilerOptions[Id]
  val backendOptions: backend.BackendOptions[Id, I]
}

object BuildInfo {


  def apply[I](backend: Backend)(project: ProjectInfoFormat[Id, I], compilerOptions: CompilerOptions[Id], backendOptions: backend.BackendOptions[Id, I]): BuildInfo[I] = {
    val backend2: backend.type = backend
    val project2 = project
    val compilerOptions2 = compilerOptions
    val backendOptions2 = backendOptions

    new BuildInfo[I] {
      override val backend: backend2.type = backend2
      override val project: ProjectInfoFormat[Id, I] = project2
      override val compilerOptions: CompilerOptions[Id] = compilerOptions2
      override val backendOptions: backend.BackendOptions[Id, I] = backendOptions2
    }
  }

  private val projectKey = "project"
  private val compilerOptionsKey = "compiler-options"

  def loadFile(file: File): IO[Throwable, Option[Vector[BuildInfo[File]]]] =
    FileOperations.readAllText(file).flatMap { buildFile =>
      Toml.parse(buildFile)
        .toOption
        .flatMap { rootTable =>
          val proj = loadProjectOpt(rootTable.values.get(projectKey))
          val compOpts = loadCompilerOptionsOpt(rootTable.values.get(compilerOptionsKey))

          (rootTable.values - projectKey - compilerOptionsKey).toVector.traverse {
            case (key, value: toml.Value.Tbl) =>
              loadBuildInfo(file, key, value, proj, compOpts)

            case (_, _) => None
          }
        }
        .traverse { _.traverse(loadProjectFile(file.getParentFile)) }
    }

  private def loadProjectOpt(proj: Option[toml.Value]): ProjectInfoFormat[Option, String] =
    proj
      .collect { case tbl: toml.Value.Tbl => tbl }
      .flatMap(loadProject)
      .getOrElse { ProjectInfoFormat[Option, String](None, None) }

  private def loadProject(proj: toml.Value.Tbl): Option[ProjectInfoFormat[Option, String]] =
    Toml.parseAs[ProjectInfoFormat[Option, String]](proj).toOption

  private def loadCompilerOptionsOpt(opts: Option[toml.Value]): CompilerOptions[Option] =
    opts
      .collect { case tbl: toml.Value.Tbl => tbl }
      .flatMap(loadCompilerOptions)
      .getOrElse { CompilerOptions[Option](None) }

  private def loadCompilerOptions(opts: toml.Value.Tbl): Option[CompilerOptions[Option]] =
    Toml.parseAs[CompilerOptions[Option]](opts).toOption

  private def loadBuildInfo(file: File, backendName: String, table: toml.Value.Tbl, globalProj: ProjectInfoFormat[Option, String], globalOptions: CompilerOptions[Option]): Option[BuildInfo[String]] =
    Backend.find(backendName).flatMap { backend =>
      val proj = loadProjectOpt(table.values.get(projectKey))
      val compOpts = loadCompilerOptionsOpt(table.values.get(compilerOptionsKey))

      val table2 = toml.Value.Tbl(table.values - projectKey - compilerOptionsKey)

      backend.parseBackendOptions(table2).toOption.map { backendOptions =>
        val compilerOpts = CompilerOptions[Id](
          moduleName = compOpts.moduleName.orElse(globalOptions.moduleName).getOrElse(FilenameUtils.getBaseName(file.getName)),
        )

        BuildInfo(backend)(
          project = ProjectInfoFormat[Id, String](
            inputFiles = globalProj.inputFiles.toList.flatten ++ proj.inputFiles.toList.flatten,
            references = globalProj.references.toList.flatten ++ proj.references.toList.flatten,
          ),
          compilerOptions = compilerOpts,
          backendOptions = backend.inferBackendOptions(compilerOpts, backendOptions),
        )
      }
    }

  private def loadProjectFile(dir: File)(build: BuildInfo[String]): IO[Throwable, BuildInfo[File]] = {
    import ProjectLoader.Implicits._

    type ProjFormat[A] = ProjectInfoFormat[Id, A]
    implicit val fileHandler = ProjectFileHandler.fileHandlerFile(dir)

    for {
      resolvedProj <- ProjectLoader[ProjFormat[String], ProjFormat[File], File].loadProject(build.project)
      resolvedBackendOpts <- build.backend.projectLoader.loadProject(build.backendOptions)
    } yield BuildInfo(build.backend)(
      project = resolvedProj,
      compilerOptions = build.compilerOptions,
      backendOptions = resolvedBackendOpts,
    )
  }
}
