package dev.argon.compiler_tests

import dev.argon.compiler.*
import dev.argon.backend.{Backend, CodeGenerator, TestExecutor}
import zio.*
import dev.argon.vm.resource.VmIrResource
import dev.argon.build.{TubeImporterImpl, LogReporter, BuildFailed}
import dev.argon.io.PathLike

trait BackendLibraryOutputProvider[Output] {
  val testExecutor: TestExecutor[TestError, Output]

  def getLibraryOutput(tubeName: TubeName): IO[TestError, testExecutor.TestProgram]
}

object BackendLibraryOutputProvider {
  type Aux[Output, Exec <: TestExecutor[TestError, Output]] = BackendLibraryOutputProvider[Output] {
    val testExecutor: Exec
  }

  def make(backend: Backend[TestError], exec: TestExecutor[TestError, backend.Output]): ZIO[ArgonLibraryProvider, Nothing, BackendLibraryOutputProvider.Aux[backend.Output, exec.type]] =
    for
      libProvider <- ZIO.service[ArgonLibraryProvider]
    yield new BackendLibraryOutputProvider[backend.Output] {
      override val testExecutor: exec.type = exec

      override def getLibraryOutput(tubeName: TubeName): IO[TestError, testExecutor.TestProgram] =
        ZIO.scoped {
          val program = libProvider.getIrLibrary(tubeName).decode[VmIrResource]
          val library = ArgonLibraries.allLibraries(tubeName)
          for
            options <- ZIO.fromEither(
              library.codeGenOptionsProvider(PathLike.join(PathLike.fromString("libraries"), tubeName.encode))
                .getOptionsForBackend(backend)
                .toRight { TestException(s"Could create options for backend ${backend.name} and library ${tubeName}") }
            )

            depLibraries = library.references.view.map { refLibName =>
              libProvider.getIrLibrary(refLibName).decode[VmIrResource].withError[TestError]
            }.toSeq

            output <- (backend.codeGenerator : (backend.codeGenerator.type & CodeGenerator[TestError, backend.Output])) match {
              case codeGenerator: (backend.codeGenerator.type & CodeGenerator.LibraryCodeGenerator[TestError, backend.Output]) =>
                for
                  opts <- codeGenerator.optionParser.parse(options)
                  out <- codeGenerator.codegen(opts, program, depLibraries)
                yield out
            }

            output <- exec.toTestProgram(output)
          yield output
        }
    }
}
