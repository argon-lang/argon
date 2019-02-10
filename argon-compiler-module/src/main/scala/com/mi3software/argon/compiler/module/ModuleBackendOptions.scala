package com.mi3software.argon.compiler.module

final case class ModuleBackendOptionsFileFormat
(
  referenceModule: String,
)

final case class ModuleBackendOptions[F[_], I]
(
  referenceModule: F[I],
)
