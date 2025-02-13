package dev.argon.compiler_tests

import dev.argon.io.DirectoryResource
import dev.argon.source.ArgonSourceCodeResource
import dev.argon.compiler.TubeName
import dev.argon.io.PathLike

trait ArgonLibraryInfo {
  val tubeName: TubeName
  val references: Set[TubeName]

  def optionsProvider(libPath: PathLike): BackendOptionsProvider
}
