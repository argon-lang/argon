package dev.argon.backend.module

final case class ModuleBackendOptionsFileFormat
(
  referenceModule: String,
)

final case class ModuleBackendOptions[F[_], I]()

final case class ModuleOutputOptions[F[_], I]
(
  referenceModule: F[I],
)
