package dev.argon.backend.module

import dev.argon.project.SingleFile

final case class ModuleBackendOptions[F[_], I]()

final case class ModuleOutputOptions[F[_], I]
(
  referenceModule: F[SingleFile[I]],
)
