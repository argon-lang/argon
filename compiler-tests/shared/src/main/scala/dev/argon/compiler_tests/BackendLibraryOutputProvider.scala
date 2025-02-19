package dev.argon.compiler_tests

import dev.argon.compiler.*
import dev.argon.backend.{Backend, CodeGenerator}
import zio.*
import dev.argon.vm.resource.VmIrResource
import dev.argon.build.{TubeImporterImpl, LogReporter, BuildFailed}

trait BackendLibraryOutputProvider {
  val backend: Backend[TestError]
  val testExecutor: TestExecutor.Aux[backend.type]

  def getLibraryOutput(tubeName: TubeName): IO[TestError, testExecutor.TestProgram]
}

object BackendLibraryOutputProvider {
  type Aux[B <: Backend[TestError], Exec <: TestExecutor.Aux[B]] = BackendLibraryOutputProvider {
    val backend: B
    val testExecutor: Exec
  }

  def make(b: Backend[TestError], exec: TestExecutor.Aux[b.type]): ZIO[ArgonLibraryProvider, Nothing, BackendLibraryOutputProvider.Aux[b.type, exec.type]] =
    for
      libProvider <- ZIO.service[ArgonLibraryProvider]
    yield new BackendLibraryOutputProvider {
      override val backend: b.type = b
      override val testExecutor: exec.type = exec

      override def getLibraryOutput(tubeName: TubeName): IO[TestError, testExecutor.TestProgram] =
        ZIO.scoped(
          for
            options <- ZIO.fromEither(
              TestCaseBackendOptions.provider
                .getOptionsForBackend(backend)
                .toRight { TestException(s"Could create options for backend ${b.name}") }
            )

            program = libProvider.getIrLibrary(tubeName).decode[VmIrResource]

            library = ArgonLibraries.allLibraries(tubeName)

            depLibraries = library.references.view.map { refLibName =>
              refLibName -> libProvider.getIrLibrary(refLibName).decode[VmIrResource]
            }.toMap

            output <- (b.codeGenerator : (b.codeGenerator.type & CodeGenerator[TestError, b.Output])) match {
              case codeGenerator: (b.codeGenerator.type & CodeGenerator.LibraryCodeGenerator[TestError, b.Output]) =>
                codeGenerator.codegen(options, program, depLibraries)
            }
            output <- exec.toTestProgram(output)
          yield output
        )
    }
}
