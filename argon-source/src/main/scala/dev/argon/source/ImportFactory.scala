package dev.argon.source

import dev.argon.compiler.{ErasedSignature, ImportSpecifier}

trait ImportFactory {
  def getImportSpecifier(sig: ErasedSignature): ImportSpecifier
}
