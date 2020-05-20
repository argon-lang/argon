package dev.argon.backend.js

import dev.argon.compiler.options.SingleFile
import dev.argon.compiler.options.{FileList, SingleFile}

final case class JSBackendOptions[F[_], I]
(
  extern: F[FileList[I]],
  inject: JSInjectCode[F, I],
)

final case class JSInjectCode[F[_], I]
(
  before: F[Option[SingleFile[I]]],
  after: F[Option[SingleFile[I]]],
)

final case class JSOutputOptions[F[_], I]
(
  outputFile: F[SingleFile[I]],
)
