package dev.argon.source

import dev.argon.io.DirectoryResource
import dev.argon.compiler.TubeName


final case class SourceCodeTubeOptions[E](
  name: TubeName,
  referencedTubes: Set[TubeName],
  sources: Seq[DirectoryResource[E, ArgonSourceCodeResource]],
)
