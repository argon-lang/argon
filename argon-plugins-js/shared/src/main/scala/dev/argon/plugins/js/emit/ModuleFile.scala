package dev.argon.plugins.js.emit

import dev.argon.compiler.module.ModulePath

private[emit] final case class ModuleFile(dir: Seq[String], file: String, path: ModulePath)
