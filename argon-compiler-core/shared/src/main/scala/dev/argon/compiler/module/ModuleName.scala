package dev.argon.compiler.module

import dev.argon.compiler.pkg._

final case class ModuleName(packageName: PackageName, path: ModulePath) derives CanEqual