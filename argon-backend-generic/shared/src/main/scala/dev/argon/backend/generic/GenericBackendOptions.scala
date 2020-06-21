package dev.argon.backend.generic

import dev.argon.compiler.options.SingleFile

final case class GenericBackendOptions[F[_], I]()

final case class GenericOutputOptions[F[_], I]
(
  referenceModule: F[Option[SingleFile[I]]],
  declarationModule: F[Option[SingleFile[I]]],
)
