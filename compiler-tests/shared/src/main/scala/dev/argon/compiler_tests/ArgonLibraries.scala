package dev.argon.compiler_tests

import dev.argon.compiler.*
import dev.argon.io.*
import dev.argon.source.ArgonSourceCodeResource
import dev.argon.backend.Backend
import dev.argon.backend.backends.js.JSBackend
import dev.argon.compiler_tests.BackendOptionsProvider.OptionsFactory

object ArgonLibraries {

  lazy val allLibraries: Map[TubeName, ArgonLibraryInfo] =
    Seq(ArgonCoreLibrary)
      .map { lib => lib.tubeName -> lib }
      .toMap

  
  object ArgonCoreLibrary extends ArgonLibraryInfo {
    override val tubeName: TubeName = TubeName("Argon", "Core")
    override val references: Set[TubeName] = Set.empty

    override def optionsProvider(libPath: PathLike): BackendOptionsProvider =
      BackendOptionsProvider(
        OptionsFactory[JSBackend[TestError]](_.JSOptions(
          externs = Seq(
            PathUtil.binaryResource(PathLike.join(libPath, "js/externs.js")).decode[TextResource],
          ),
        )),
      )

  }

  

}
