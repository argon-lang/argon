package dev.argon.backend

import dev.argon.compiler.options.{CompilerOptions, FileGlob, FileList}
import dev.argon.module.PathResourceIndicator
import shapeless.{Path => _, _}

trait BuildInfo[I] {
  val backend: Backend
  val compilerOptions: CompilerOptions[Id, I]
  val backendOptions: backend.BackendOptions[Id, I]
  val outputOptions: backend.BackendOutputOptions[Id, I]
}

object BuildInfo {

  type Resolved[P] = BuildInfo[PathResourceIndicator[P]]


  def apply[I](backend: Backend)(compilerOptions: CompilerOptions[Id, I], backendOptions: backend.BackendOptions[Id, I], outputOptions: backend.BackendOutputOptionsId[I]): BuildInfo[I] = {
    val backend2: backend.type = backend
    val compilerOptions2 = compilerOptions
    val backendOptions2 = backendOptions
    val outputOptions2 = outputOptions

    new BuildInfo[I] {
      override val backend: backend2.type = backend2
      override val compilerOptions: CompilerOptions[Id, I] = compilerOptions2
      override val backendOptions: backend.BackendOptions[Id, I] = backendOptions2
      override val outputOptions: backend.BackendOutputOptionsId[I] = outputOptions2
    }
  }

}
