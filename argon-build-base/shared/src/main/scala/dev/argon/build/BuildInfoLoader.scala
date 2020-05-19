package dev.argon.build

import java.io.IOException

import dev.argon.io.Path
import dev.argon.io.Path.PathExtensions
import cats.{Id => _, _}
import cats.data.{EitherT, OptionT}
import cats.implicits._
import dev.argon.backend.Backend
import zio.{BuildInfo => _, _}
import zio.interop.catz.core._
import dev.argon.compiler._
import dev.argon.compiler.core._
import dev.argon.compiler.loaders.ResourceIndicator
import dev.argon.io.fileio.FileIO
import dev.argon.module.PathResourceIndicator
import dev.argon.project.BuildInfo.Resolved
import dev.argon.project._
import toml.Toml
import toml.Codecs._
import ExtraTomlCodecs._
import shapeless.{BuildInfo => _, Path => _, _}


object BuildInfoLoader {

  private val projectKey = "project"
  private val compilerOptionsKey = "compiler-options"
  private val backendKey = "backend"
  private val outputKey = "output"



  def loadFile[P: Path : Tagged](file: P): ZIO[FileIO[P] with BackendProvider, IOException, Either[String, Vector[Resolved[P]]]] = for {

    fileIO <- ZIO.access[FileIO[P]](_.get)
    absoluteFile <- fileIO.getAbsolutePath(file)
    currentDir <- Path.of(".")
    currentDirAbs <- fileIO.getAbsolutePath(currentDir)
    dir = absoluteFile.parent.getOrElse(currentDirAbs)
    buildFile <- fileIO.readAllText(file)
    result <-
      Toml.parse(buildFile)
        .leftMap { case (addr, message) => addr.toString + message }
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
            .traverse[EitherT[URIO[BackendProvider, *], String, *], BuildInfo[String]] {
              case (key, value: toml.Value.Tbl) =>
                EitherT(
                  ZIO.access[BackendProvider](_.get.findBackend(key).toRight { s"Could not find backend $key" })
                    .map { backendOpt =>
                      backendOpt.flatMap { backend =>
                        loadBuildInfo(file, backend, value, proj, compOpts)
                      }
                    }
                )

              case (_, _) => EitherT.fromEither(Left("Invalid value for backends"))
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

  private def loadBuildInfo[P: Path](file: P, backend: Backend, table: toml.Value.Tbl, globalProj: ProjectInfoFormat[Option, String], globalOptions: CompilerOptions[Option]): Either[String, BuildInfo[String]] = {
    val proj = loadProjectOpt(table.values.get(projectKey))
    val compOpts = loadCompilerOptionsOpt(table.values.get(compilerOptionsKey))

    val output = table.values
      .get(outputKey)
      .collect { case table: toml.Value.Tbl => table }
      .getOrElse { toml.Value.Tbl(Map.empty) }

    for {

      outputOpts <- backend.parseOutputOptions(output).leftMap { case (addr, message) => addr.toString + message }

      table2 = toml.Value.Tbl(table.values - projectKey - compilerOptionsKey - outputKey)
      backendOptions <- backend.parseBackendOptions(table2).leftMap { case (addr, message) => addr.toString + message }

      compilerOpts = CompilerOptions[Id](
        moduleName = compOpts.moduleName.orElse(globalOptions.moduleName).getOrElse(file.fileNameWithoutExtension),
      )
    } yield BuildInfo(backend)(
      project = ProjectInfoFormat[Id, String](
        inputFiles = new FileGlob(globalProj.inputFiles.toList.flatMap(_.files) ++ proj.inputFiles.toList.flatMap(_.files)),
        references = new FileList(globalProj.references.toList.flatMap(_.files) ++ proj.references.toList.flatMap(_.files)),
      ),
      compilerOptions = compilerOpts,
      backendOptions = backend.inferBackendOptions(compilerOpts, backendOptions),
      outputOptions = backend.inferOutputOptions(compilerOpts, outputOpts),
    )
  }

  private def loadProjectFile[P: Path : Tagged](dir: P)(build: BuildInfo[String]): ZIO[FileIO[P], IOException, Resolved[P]] = {
    import dev.argon.project.ProjectLoader.Implicits._

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
