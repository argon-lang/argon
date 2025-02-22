package dev.argon.compiler_tests

import dev.argon.compiler.*
import dev.argon.io.*
import dev.argon.source.ArgonSourceCodeResource
import dev.argon.backend.options.OptionValue

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
        "js" -> Map(
          "externs" -> OptionValue.ManyValues(
            OptionValue.Atom.BinaryResource(PathUtil.binaryResource(PathLike.join(libPath, "js/externs.js")))
          ),
        ),
      )

  }

  

}
