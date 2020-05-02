package dev.argon.build.project

import java.io.IOException

import dev.argon.io.Path
import dev.argon.io.Path.PathExtensions
import cats._
import cats.data.OptionT
import cats.implicits._
import dev.argon.backend.{Backend, ProjectFileHandler, ProjectLoader}
import dev.argon.build._
import zio._
import zio.interop.catz._
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import toml.Toml
import toml.Codecs._
import dev.argon.util.AnyExtensions._
import shapeless.{Id => _, Path => _, _}


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
  val outputOptions: backend.BackendOutputOptions[Id, I]
}

object BuildInfo {

  type Resolved[P] = BuildInfo[PathResourceIndicator[P]]


  def apply[I](backend: Backend)(project: ProjectInfoFormat[Id, I], compilerOptions: CompilerOptions[Id], backendOptions: backend.BackendOptionsId[I], outputOptions: backend.BackendOutputOptionsId[I]): BuildInfo[I] = {
    val backend2: backend.type = backend
    val project2 = project
    val compilerOptions2 = compilerOptions
    val backendOptions2 = backendOptions
    val outputOptions2 = outputOptions

    new BuildInfo[I] {
      override val backend: backend2.type = backend2
      override val project: ProjectInfoFormat[Id, I] = project2
      override val compilerOptions: CompilerOptions[Id] = compilerOptions2
      override val backendOptions: backend.BackendOptionsId[I] = backendOptions2
      override val outputOptions: backend.BackendOutputOptionsId[I] = outputOptions2
    }
  }

  private val projectKey = "project"
  private val compilerOptionsKey = "options"
  private val backendKey = "backend"
  private val outputKey = "output"



  def loadFile[P: Path : Tagged](file: P): ZIO[FileIO[P] with BackendProvider, IOException, Option[Vector[Resolved[P]]]] = for {

    fileIO <- ZIO.access[FileIO[P]](_.get)
    absoluteFile <- fileIO.getAbsolutePath(file)
    currentDir <- Path.of(".")
    currentDirAbs <- fileIO.getAbsolutePath(currentDir)
    dir = absoluteFile.parent.getOrElse(currentDirAbs)
    buildFile <- fileIO.readAllText(file)
    result <-
      Toml.parse(buildFile)
        .toOption
        .flatTraverse { rootTable =>
          val proj = loadProjectOpt(rootTable.values.get(projectKey))
          val compOpts = loadCompilerOptionsOpt(rootTable.values.get(compilerOptionsKey))

          rootTable
            .values
            .get(backendKey)
            .collect { case table: toml.Value.Tbl => table }
            .toList
            .flatMap { _.values }
            .toVector
            .traverse[OptionT[URIO[BackendProvider, *], *], BuildInfo[String]] {
              case (key, value: toml.Value.Tbl) =>
                OptionT(
                  ZIO.access[BackendProvider](_.get.findBackend(key))
                    .map { backendOpt =>
                      backendOpt.flatMap { backend =>
                        loadBuildInfo(file, backend, value, proj, compOpts)
                      }
                    }
                )

              case (_, _) => OptionT.none
            }
            .value
        }
        .flatMap {
          _.traverse { _.traverse(loadProjectFile(dir)) }
        }
  } yield result

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

  private def loadBuildInfo[P: Path](file: P, backend: Backend, table: toml.Value.Tbl, globalProj: ProjectInfoFormat[Option, String], globalOptions: CompilerOptions[Option]): Option[BuildInfo[String]] = {
    val proj = loadProjectOpt(table.values.get(projectKey))
    val compOpts = loadCompilerOptionsOpt(table.values.get(compilerOptionsKey))

    val output = table.values
      .get(outputKey)
      .collect { case table: toml.Value.Tbl => table }
      .getOrElse { toml.Value.Tbl(Map.empty) }

    for {

      outputOpts <- backend.parseOutputOptions(output).toOption

      table2 = toml.Value.Tbl(table.values - projectKey - compilerOptionsKey)
      backendOptions <- backend.parseBackendOptions(table2).toOption

      compilerOpts = CompilerOptions[Id](
        moduleName = compOpts.moduleName.orElse(globalOptions.moduleName).getOrElse(file.fileNameWithoutExtension),
      )
    } yield BuildInfo(backend)(
      project = ProjectInfoFormat[Id, String](
        inputFiles = globalProj.inputFiles.toList.flatten ++ proj.inputFiles.toList.flatten,
        references = globalProj.references.toList.flatten ++ proj.references.toList.flatten,
      ),
      compilerOptions = compilerOpts,
      backendOptions = backend.inferBackendOptions(compilerOpts, backendOptions),
      outputOptions = backend.inferOutputOptions(compilerOpts, outputOpts),
    )
  }

  private def loadProjectFile[P: Path : Tagged](dir: P)(build: BuildInfo[String]): ZIO[FileIO[P], IOException, Resolved[P]] = {
    import dev.argon.backend.ProjectLoader.Implicits._

    type ProjFormat[A] = ProjectInfoFormat[Id, A]
    implicit val fileHandler = ProjectFileHandler.fileHandlerPath(dir)

    for {
      resolvedProj <- ProjectLoader[ProjFormat[String], ProjFormat[PathResourceIndicator[P]], String, PathResourceIndicator[P]].loadProject[FileIO[P], IOException](build.project)
      resolvedBackendOpts <- build.backend.backendOptionsProjectLoader[String, PathResourceIndicator[P]].loadProject[FileIO[P], IOException](build.backendOptions)
      resolvedOutputOpts <- build.backend.outputOptionsProjectLoader[String, PathResourceIndicator[P]].loadProject[FileIO[P], IOException](build.outputOptions)
    } yield BuildInfo(build.backend)(
      project = resolvedProj,
      compilerOptions = build.compilerOptions,
      backendOptions = resolvedBackendOpts,
      outputOptions = resolvedOutputOpts,
    )
  }
}
