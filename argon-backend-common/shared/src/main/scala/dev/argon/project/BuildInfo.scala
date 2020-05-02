package dev.argon.project

import dev.argon.backend.Backend
import dev.argon.compiler._
import dev.argon.module.PathResourceIndicator
import shapeless.{Path => _, _}


final case class ProjectInfoFormat[F[_], I]
(
  inputFiles: F[FileGlob[I]],
  references: F[FileList[I]],
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

}
