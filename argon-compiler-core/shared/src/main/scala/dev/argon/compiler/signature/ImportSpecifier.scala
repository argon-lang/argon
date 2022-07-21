package dev.argon.compiler.signature

import dev.argon.compiler.module.{ModuleName, ModulePath}
import dev.argon.compiler.signature.ErasedSignature
import dev.argon.compiler.tube.TubeName
import dev.argon.parser.IdentifierExpr

final case class ImportSpecifier(tube: TubeName, module: ModulePath, name: IdentifierExpr, signature: ErasedSignature) derives CanEqual {
  def moduleName: ModuleName = ModuleName(tube, module)
}
