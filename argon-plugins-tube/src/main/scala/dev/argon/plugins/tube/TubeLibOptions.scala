package dev.argon.plugins.tube

import dev.argon.compiler.Context
import dev.argon.io.ZipFileResource

final case class TubeLibOptions[-R, +E, TContext <: Context, ContextOptions]
(
  tube: ZipFileResource[R, E],
)
