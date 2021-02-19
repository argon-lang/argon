package dev.argon.backend.jvm

import dev.argon.options.{FileList, SingleFile}

final case class JVMBackendOptions[F[_], I]
(
  jdkVersion: F[String],
  systemModules: F[List[String]],
  extern: F[FileList],
 )

final case class JSOutputOptions[F[_], I]
(
  outputFile: F[SingleFile],
)
