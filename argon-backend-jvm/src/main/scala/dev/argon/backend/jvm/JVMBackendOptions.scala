package dev.argon.backend.jvm

import dev.argon.compiler.options.SingleFile
import dev.argon.compiler.options.{FileList, SingleFile}

final case class JVMBackendOptions[F[_], I]
(
  jdkVersion: F[String],
  systemModules: F[List[String]],
  extern: F[FileList[I]],
 )

final case class JSOutputOptions[F[_], I]
(
  outputFile: F[SingleFile[I]],
)
