package dev.argon.tube.encoder

import dev.argon.compiler.ImportSpecifier

final case class SyntheticImport(
  parent: ImportSpecifier | SyntheticImport,
  index: BigInt,
) derives CanEqual
