package dev.argon.compiler

import dev.argon.compiler.core._

final case class CompilerOptions[F[_]]
(
  moduleName: F[String],
)
